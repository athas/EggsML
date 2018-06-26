  
CSV ("comma separated values") files
====================================
The "comma" separated files contain the same information as the owl/rdf
files but are structured as table-like columns separated by '@'. This is
primarily intended for import in a (relational) database, and may
allow for queries like the following (assuming the DanNet resource is 
imported as suggested below):
  ex. 1 "All synsets linked to the wordform 'spand'":
     SELECT w.form, ws.register, s.id, s.gloss, s.ontological_type
     FROM synsets s, words_synsets ws, words w
     WHERE s.id = ws.synset_id
       AND ws.word_id = w.id
       AND ww.form = 'spand';
    Outputs:
      spand |       |  2928 | åben, keglestub- el. cylinderformet ... | Container+Artifact+Object |
      spand | slang |  2148 | gammel og dårlig bil                    | Vehicle+Object+Artifact   |
      spand |       |  5697 | gruppe af trækdyr spændt for en vogn... | Animal+Object+Group       |
      ...
      
  ex. 2 "All relations that describe the synset with id 2148 ({spand,1_2})":
     SELECT name, target, taxonomic FROM relations WHERE synset_id = 2148;
    Outputs:
      hyponymOf         | 1507 | nontaxonomic |
      involvedAgent     | 2119 |              |
      locationMeronymOf | 7587 |              |
      madeBy            | 1656 |              |
      ...

Reverse relations
-----------------
No attempt has been made to reverse the reversable relations: For 
instance the hyponymOf relation is the reverse of the hypernymOf 
relation, and if only the synset for '{herring_1}' is described as 
"hyponymOf fish_1", {fish_1} is not necessarily described as 
"hypernymOf herring_1". Unlike the owl/rdf files no reasoner will 
automatically compute the reverse relations. We refer to the list 
of relations and how they relate to each other at the end of this 
file. 

FILES
=====
Files included in this release:

Meta files
----------
README.txt  This file.
LICENSE.txt The license that the user must accept when using DanNet.
VERSION.txt This version of the DanNet resource.
            Check http://wordnet.dk/download_html for updates.
RELEASE_NOTES.txt Changes in this release.

The DanNet resource
-------------------
synsets.cvs
    id:    Id of synset. This id will remain constant in future versions
           of DanNet.
    label: Name of synset based on the word forms linked to this synset.
           It is only intended as a convenience for the user. For
           information about the lexical forms, please refer to words.csv.
    gloss: Gloss of the synsets. Consists of a small part of the definition
           from the Danish Dictionary plus hand-selected examples from the
           corpus.
    ontological_type: Ontological type of the synset, e.g. 'Comestible'
           or 'Vehicle+Object+Artifact'.

dummies.csv
           The dummies file has exactly the same structure as the synsets
           file. But synsets in this file are not supplied with any
           relations in this version of DanNet.

relations.csv:
    synset_id: Id of the synset is described by the relation.
    name:      The name of the relation (in wordnet/owl terminology)
    name2:     the name of the relation (i EuroWordNet terminology)
    value:     The target of the relation. The Value is always an id 
               of a synset (in the synsets.csv file), a dummy (in
               the dummies.csv file), or a Princeton Wordnet synset.
    taxonomic: Possible values: 'taxonomic' or 'nontaxonomic'
               Distinguishes between taxonomic and nontaxonomic
               hyponymy, cf. the specifications for DanNet 
               (http://wordnet.dk/download_html). Available only in
               Danish. Only relevant for the hyponymOf relation.
    inheritance_comment: A synset inherits relations from hypernyms
               If a relation is inherited rather than supplied for the
               particular synset, a text comment will state from which 
               synset the relation stems.
               
words.csv:
    id:    Id for the lexical entry. This will be stable through future
           versions of DanNet. However, for some id's a number has been 
           appended to the core id with a hyphen (e.g. '...-1'. This part
           of the id might unfortunately in rare cases change in future 
           releases, while the core part will be stable.
    form:  The lexical form of the entry
    pos:   The part of speech of the entry


wordsenses.csv:
    wordsense_id: Id of the lexical wordsense. Note the id is not unique
                  as a wordsense may to connected to more than one
                  synset.
    word_id:   Id of the lexical entry (in the words.csv file)
    synset_id: Id of the synset (in the synsets.csv file)
    register:  Some word senses may be marked as non-standard, e.g.
               'sj.' for seldomly used, 'gl.' for old-fashionable,
               or 'slang'.
               In general, if a value is present for a word sense in 
               this column, it may be regarded as non-standard use. 

Table of relations
=========================
EUROWORDNET/DANNET  WORDNETOWL         REVERSE OF         KIND OF
---------------------------------------------------------------------------
concerns            concerns 
used_for            usedFor
used_for_object     usedForObject 
has_holonym         meronymOf          has_meronym     
has_holo_location   locationMeronymOf  has_mero_location  has_holonym
has_holo_madeof     madeofMeronymOf    has_mero_madeof    has_holonym
has_holo_member     memberMeronymOf    has_mero_member    has_holonym
has_holo_part       partMeronymOf      has_mero_part      has_holonym
has_hyperonym       hyponymOf          has_hyponym
has_hyponym         hypernymOf         has_hyperonym
has_meronym         holonymOf          has_holonym
has_mero_location   locationHolonymOf  has_holo_location  has_meronym
has_mero_madeof     madeofHolonymOf    has_holo_madeof    has_meronym
has_mero_member     memberHolonymOf    has_holo_member    has_meronym
has_mero_part       partHolonymOf      has_holo_part      has_meronym
involved_agent      involvedAgent
involved_patient    involvedPatient
made_by             madeBy
near_synonym        nearSynonymOf
role_agent          roleAgent
role_patient        rolePatient
