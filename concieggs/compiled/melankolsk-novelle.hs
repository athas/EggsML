module Main (main) where

import System.Environment (getArgs)
import Control.Monad
import Control.Monad.Reader
import Control.Applicative
import Data.List
import Concieggs.Monad
import Concieggs.Random


data MelankolskKontekst = MelankolskKontekst { bekendtNavn :: String }
  deriving (Show)

type Tilfældig a = RandomT (Reader MelankolskKontekst) a

liftTilfældig :: MelankolskKontekst -> Tilfældig a -> ConcieggsM a
liftTilfældig k t = flip runReader k <$> (liftIO $ evalRandTIO t)


class Generering a where
  generér :: Tilfældig a

class Formatering a where
  formatér :: a -> Tilfældig String


randElems :: Int -> Int -> Tilfældig a -> Tilfældig [a]
randElems a b m = do
  nElems <- randomR a b
  replicateM nElems m

navn :: Random String
navn = choiceM [ choice ["Je", "Ka", "Lo"]
                 <++> choiceM [ choice ["tt", "ll", "nn"]
                                <++> pure "e"
                              , pure "ren"
                              ]
               , choice ["Am", "Em"]
                 <++> choice ["a", "e"]
                 <++> choice ["lie", "tt"]
               ]

nogetBestemt :: String -> Tilfældig String
nogetBestemt s = do
  navn <- bekendtNavn <$> ask
  præ <- weightedChoice [ (navn ++ "s", 4)
                        , ("hendes", 1)
                        , ("hans", 1)
                        , ("deres", 1)
                        , ("min fars", 1)
                        , ("min mors", 1)
                        ]
  return (præ ++ " " ++ s)

kommatér :: [String] -> String
kommatér [] = ""
kommatér [s] = s
kommatér (s : ss) = s ++ kommatérFlere ss
  where kommatérFlere [s] = " og " ++ s
        kommatérFlere (s : ss) = ", " ++ s ++ kommatérFlere ss


data BlankTing = Pupiller
               | LakeretBord
               | TomPose
               deriving (Show)

instance Generering BlankTing where
  generér = choice [Pupiller, LakeretBord, TomPose]

instance Formatering BlankTing where
  formatér Pupiller = nogetBestemt "pupiller"
  formatér LakeretBord = nogetBestemt "lakerede bord"
  formatér TomPose = do
    supermarked <- choice ["Netto", "Fakta", "Kiwi", "Meny"]
    nogetBestemt ("tomme " ++ supermarked ++ "-pose")


data EnsformigTing = BladenePåTræerne
                   | Spejlet
                   | TomhedenI [BlankTing]
                   deriving (Show)

instance Generering EnsformigTing where
  generér = choiceM [ pure BladenePåTræerne
                    , pure Spejlet
                    , TomhedenI <$> randElems 1 3 generér
                    ]

instance Formatering EnsformigTing where
  formatér BladenePåTræerne = do
    træsort <- choice ["bøge", "birke", "aske", "nåle"]
    return ("bladene på " ++ træsort ++ "træerne")
  formatér Spejlet = pure "spejlet"
  formatér (TomhedenI xs) =
    nogetBestemt =<< (pure "tomhed i " <++> (kommatér <$> mapM formatér xs))


data HyggeligTing = Kage
                  | GodtTV
                  deriving (Show)

instance Generering HyggeligTing where
  generér = choice [Kage, GodtTV]

instance Formatering HyggeligTing where
  formatér Kage = nogetBestemt =<< choice [ "chokoladekage"
                                          , "roulade"
                                          , "flødeskumskager fra bageren"
                                          ]
  formatér GodtTV = choice [ "Ole Stephensen i Go' Morgen Danmark"
                           , "Matador i fjernsynet"
                           , "Hugo i morgen-tv"
                           , "tv-værter der talte ordentligt dansk"
                           , "gode rejseannoncer på tekst-tv"
                           ]

data DumTing = Mobiltelefon
             | SocialeMedier
             | Karrierrepolitikere
             deriving (Show)

instance Generering DumTing where
  generér = choice [Mobiltelefon, SocialeMedier, Karrierrepolitikere]

instance Formatering DumTing where
  formatér Mobiltelefon = nogetBestemt "mobiltelefon"
  formatér SocialeMedier = pure "de sociale medier"
  formatér Karrierrepolitikere = pure "karrierepolitikerne"


data GodtSted = BonBonLand
              | ForanFjernsynet
              deriving (Show)

instance Generering GodtSted where
  generér = choice [BonBonLand, ForanFjernsynet]

instance Formatering GodtSted where
  formatér BonBonLand = pure "i BonBon-Land"
  formatér ForanFjernsynet = pure "foran fjernsynet"


data GamleDage = DengangMed HyggeligTing
               | DengangUden [DumTing]
               | DaViVar GodtSted
               deriving (Show)

instance Generering GamleDage where
  generér = choiceM [ DengangMed <$> generér
                    , DengangUden <$> randElems 1 3 generér
                    , DaViVar <$> generér
                    ]

instance Formatering GamleDage where
  formatér (DengangMed t) =
    pure "dengang med " <++> formatér t
  formatér (DengangUden ts) =
    pure "dengang uden " <++> (kommatér <$> mapM formatér ts)
  formatér (DaViVar t) =
    pure "da vi var " <++> formatér t


data StilleSted = EnAllé
                | Gangtunnellerne
                | EnKælder
                deriving (Show)

instance Generering StilleSted where
  generér = choice [EnAllé, Gangtunnellerne, EnKælder]

instance Formatering StilleSted where
  formatér EnAllé = do
    sted <- choice ["parken", "Amalienborg", "søbredden"]
    pure ("alléen ved " ++ sted)
  formatér Gangtunnellerne = pure "gangtunnellerne"
  formatér EnKælder = nogetBestemt "kælder"


data AtSiddeFastILivet = StirrePå EnsformigTing
                       | TænkeOver GamleDage
                       | GåVed StilleSted
                       deriving (Show)

instance Generering AtSiddeFastILivet where
  generér = choiceM [ StirrePå <$> generér
                    , TænkeOver <$> generér
                    , GåVed <$> generér
                    ]

instance Formatering AtSiddeFastILivet where
  formatér (StirrePå t) = pure "Jeg stirrede på " <++> formatér t <++> pure "."
  formatér (TænkeOver t) = pure "Jeg tænkte over " <++> formatér t <++> pure "."
  formatér (GåVed t) = pure "Jeg gik ved " <++> formatér t <++> pure "."


data AtHaveDetHalvskidt = AtHaveDetHalvskidt
                        deriving (Show)

instance Generering AtHaveDetHalvskidt where
  generér = pure AtHaveDetHalvskidt

instance Formatering AtHaveDetHalvskidt where
  formatér AtHaveDetHalvskidt =
    pure "Jeg "
    <++> choice ["var", "blev"]
    <++> pure " "
    <++> choice ["træt", "ked", "bitter", "voldsomt melankolsk"]
    <++> pure "."


data MelankolskNovelle = MelankolskNovelle { novellestart :: AtHaveDetHalvskidt
                                           , novellemidte :: [AtSiddeFastILivet]
                                           , novelleslut :: AtHaveDetHalvskidt
                                           }
                       deriving (Show)

instance Generering MelankolskNovelle where
  generér = MelankolskNovelle <$> generér <*> (randElems 2 6 generér) <*> generér

instance Formatering MelankolskNovelle where
  formatér (MelankolskNovelle start midte slut) =
    intercalate "  " <$> sequence ([formatér start]
                                   ++ map formatér midte
                                   ++ [formatér slut])


skrivMelankolskNovelle :: ConcieggsM ()
skrivMelankolskNovelle = do
  kontekst <- MelankolskKontekst <$> liftRand navn
  novelle <- liftTilfældig kontekst (generér :: Tilfældig MelankolskNovelle)
  -- echo $ show novelle
  text <- liftTilfældig kontekst $ formatér novelle
  echo text

main :: IO ()
main = runConcieggsM skrivMelankolskNovelle
