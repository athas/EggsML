# -*- encoding: utf-8 -*-

from eggsml import eggsml
import concieggs_vm
import random

class eggsml_page:
    e = None
    
    def aliases(self):
        random.seed()
        al = self.e.get_aliases()
        l = '<h2>Aliaser</h2>\n<ul>\n'
        for a in al:
            l += '<li>'
            w = random.randint(0, len(a)-1)
            random.shuffle(a)
            i = 0
            t = ''
            for an in a:
                if i == w:
                    t += '<strong>%s</strong>, ' %an
                else:
                    t += '%s, ' % an
                i+=1
            l += '%s</li>\n' % t.strip().strip(',')
        l += '</ul>\n'
        return l
    
    def purchases(self):
        pl = self.e.get_purchases()
        pl.reverse()
        n = len(pl)
        l = '<h2>Indkøb</h2>\n'
        l += '<table id="eggspurchases">\n<thead><tr>\n<th>Dato</th><th>Indkøber</th><th>Kroner</th>\n</tr></thead><tbody>\n'
        i = 0
        transactions=[]
        while i < n:
            if i+1 < n and pl[i]['amount']==pl[i+1]['amount']*-1:
                if pl[i]['amount'] < 0:
                    transactions.append({'date': pl[i]['date'],
                                 'from': pl[i]['alias'],
                                 'to': pl[i+1]['alias'],
                                 'amount': pl[i+1]['amount']})
                else:
                    transactions.append({'date': pl[i]['date'],
                                 'from': pl[i+1]['alias'],
                                 'to': pl[i]['alias'],
                                 'amount': pl[i]['amount']})
                i += 2
            else:
                p=pl[i]
                l += '<tr>\n<td>%s</td><td>%s</td><td>%s</td>\n</tr>\n' % (p['date'], p['alias'], self.currency(p['amount']))
                i += 1
        l += '</tbody></table>\n'
        l += '<h2>Overførsler</h2>\n'
        l += '<table id="eggstransactions">\n<thead><tr>\n<th>Dato</th><th>Fra</th><th>Til</th><th>Kroner</th>\n</tr></thead><tbody>\n'
        for t in transactions:
            l += '<tr>\n<td>%s</td><td>%s</td><td>%s</td><td>%s</td>\n</tr>\n' % (t['date'], t['from'], t['to'], self.currency(t['amount']))
        l += '</tbody></table>\n'
        return l

    def negative(self, b):
        return (' class="negative"' if b<0 else ' class="positive"')

    def niceDays(self, daycount):
        if daycount == 0:
            return "I dag"
        elif daycount == 1:
            return "I går"
        else:
            return str(daycount) + " dage siden"

    def balances(self):
        userinfo = self.e.get_userinfo()
        uinfsorted0 = sorted(userinfo.iteritems(), key=lambda (k,v):v['eggscount'], reverse=True)
        uinfsorted1 = sorted(uinfsorted0, key=lambda (k,v): -v['lasteggs'], reverse=True)
        l = '<h2>Saldoer</h2>\n'
        alumne = '<h2>Alumne</h2>\n'
        header = '<thead>\n<tr>\n<th>Bruger</th><th>Saldo</th><th>Betalt ialt</th><th>Måltider</th><th>Gns. pris</th><th>Seneste eggs</th>\n</tr>\n</thead><tbody>\n'
        l += '<table id="eggsdata" class="tablesorter">' + header
        alumne += '<table id="alumne" class="tablesorter">' + header
        totalpaid = 0.0
        totalcount = 0.0
        new_total = 0
        for v in userinfo:
            new_total += userinfo[v]['balance']
        for u in uinfsorted1:
            alias = u[0]
            data = u[1]
            balance = data['balance']
            latest_lunch = self.niceDays(data['lasteggs'])
            avg_paid = (data['paid'] - balance)/data['eggscount'];
            row = '<tr>\n<td>%s</td><td%s>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td>\n</tr>\n' % (alias, self.negative(balance), self.currency(balance),
                                                               self.currency(data['paid']), self.pointer(data['eggscount']),
                                                               self.currency(avg_paid),
                                                               latest_lunch)
            if data['lasteggs'] > 30 and abs(balance) < 10:
                alumne += row
            else:
                l += row
            totalpaid += data['paid']
            totalcount += data['eggscount']
        #total_avg = (paid - data['totalpaid'])/data['totalcount'];
        l += '<tfoot><tr class="total">\n<td>Total (m. alumne)</td><td%s>%s</td><td>%s</td><td>%s</td><td>%s</td><td>&nbsp;</td>\n</tr></tfoot>\n' % (self.negative(new_total), self.currency(new_total),
                                                            self.currency(totalpaid), self.pointer(totalcount),
                                                            self.currency(self.e.get_average_price()))
        l += '</tbody></table>\n'
        alumne += '</tbody></table>\n'
        l += '<h3>Gennemsnitlig måltidspris: %s</h3>\n' % self.currency(self.e.get_average_price())
#        l += '<div id="graph"><embed src="./graph.aspeggs" width="650" height="50" type="image/svg+xml" pluginspage="http://getfirefox.com" /></div>\n'
        url = self.constructChartURL();
        graf = '<div id="graph"><img src="' + url + '" alt="Måltidsgraf" title=""/></div>\n'
        return l + graf + alumne

    def constructChartURL(self):
        url = 'http://chart.apis.google.com/chart'
        url += '?cht=p' # Chart type
        url += '&chs=750x400' # Chart size
        balances = self.e.get_userinfo()
        users = sorted(balances.iteritems(), key=lambda (k,v):v['eggscount'], reverse=True)
        values = []
        colours = []
        aliases = []
        novicetotalcount = 0
        for u in users:
          alias = u[0]
          data = u[1]
          if data['eggscount'] >= 50:
            aliases.append(alias)
            values.append(data['eggscount'])
            colours.append(self.e.get_colour(alias)[1:])
          else:
            novicetotalcount += data['eggscount'];

        aliases.append('Novices (<50 eggs)')
        values.append(novicetotalcount)
        colours.append('878787')

        
        url += '&chd=t:%s' % ",".join([str(s) for s in values]) # 52,47,46,47,117.5,191.5,86.5
        url += '&chds=0,%s' % str(max(values)) # Scale values
        url += '&chl=%s' % "|".join(aliases) # Aliases
        url += '&chco=%s' % "|".join(colours) # Colour spec
        return url    
    
    def wishes(self):
        wl = self.e.get_wishes()
        l = '<h2 id="Ønsker">Indkøbsønsker</h2>\n'
        l += '<ul>\n'
        for w in wl:
            l += '<li>%s</li>\n' % w
        l += '</ul>\n'
        return l
        
    def index(self):
        o = '<h1>Brainfuck pr&aelig;senterer Brainfuck\'s EggsML</h1>'
        o += '<h2>Nu uden Brainfuck!</h2>'
        o += '<a href="./graph_timeline.aspeggs">Se graf over deltagelse til Eggs</a>.'
        o += '<br/><a href="./graph_timeline2.aspeggs">Eggs Enterprise Beta v2 graf</a>.'
        
        o += "<h2>Neggst 'n' preggs</h2>" + "<p>" + self.neggst() + "</p>" + "<p>" + self.preggs() + "</p>"
        o += self.aliases()
        o += self.balances()
        o += self.wishes()
        o += self.purchases()
        self.output = o
        return o
    
    def neggst(self): 
        return concieggs_vm.neggst()
    
    def preggs(self):
        return concieggs_vm.preggs()
    
    def currency(self, i):
        i = str(round(i,2)).split('.')
        if len(i[1])==1:
            i[1] = i[1]+'0'
        return '%s,%s' % (i[0], i[1])
    
    def pointer(self, i):
        return str(i).replace('.', ',')
    
    def __init__(self):
        self.e = eggsml()
        self.e.parse('slashdotfrokost')
