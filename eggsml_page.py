# -*- encoding: utf-8 -*-

from eggsml import eggsml

class eggsml_page:
	e = None
	
	def aliases(self):
		al = self.e.get_aliases()
		l = '<h2>Aliaser</h2>\n<ul>\n'
		for a in al:
			l += '<li>'
			i = 0
			t = ''
			for an in a:
				if i == 0:
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
		l = '<h2>Indkøb</h2>\n'
		l += '<table>\n<tr>\n<th>Dato</th><th>Indkøber</th><th>Kroner</th>\n</tr>\n'
		for p in pl:
			l += '<tr>\n<td>%s</td><td>%s</td><td>%s</td>\n</tr>\n' % (p['date'], p['alias'], self.currency(p['amount']))
		l += '</table>\n'
		return l

	def negative(self, b):
		return (' class="negative"' if b<0 else ' class="positive"')

	def balances(self):
		balances = self.e.get_balances()
		bl = sorted(balances.iteritems(), key=lambda (k,v):v['totalcount'], reverse=True)
		l = '<h2>Saldoer</h2>\n'
		l += '<table>\n<tr>\n<th>Bruger</th><th>Saldo</th><th>Betalt ialt</th><th>Måltider</th><th>Gns. pris</th>\n</tr>\n'
		totalpaid = 0.0
		totalcount = 0.0
		new_balances = self.e.get_balances_new()
		new_total = 0
		for v in new_balances:
			new_total += new_balances[v]
		for b in bl:
			alias = b[0]
			data = b[1]
                        balance = new_balances[alias]
                        avg_paid = (data['totalpaid'] - balance)/data['totalcount'];
			l += '<tr>\n<td>%s</td><td%s>%s</td><td>%s</td><td>%s</td><td>%s</td>\n</tr>\n' % (alias, self.negative(balance), self.currency(balance),
                                                                                                           self.currency(data['totalpaid']), self.pointer(data['totalcount']),
                                                                                                           self.currency(avg_paid))
			totalpaid += data['totalpaid']
			totalcount += data['totalcount']
                #total_avg = (paid - data['totalpaid'])/data['totalcount'];
		l += '<tr class="total">\n<td>Total</td><td%s>%s</td><td>%s</td><td>%s</td><td>%s</td>\n</tr>\n' % (self.negative(new_total), self.currency(new_total),
                                                                                                                    self.currency(totalpaid), self.pointer(totalcount),
                                                                                                                    self.currency(self.e.get_average_price()))
		l += '</table>\n'
		l += '<h3>Gennemsnitlig måltidspris: %s</h3>\n' % self.currency(self.e.get_average_price())
#		l += '<div id="graph"><embed src="./graph.py" width="650" height="50" type="image/svg+xml" pluginspage="http://getfirefox.com" /></div>\n'
		url = self.constructChartURL();
		l += '<div id="graph"><img src="' + url + '" alt="Måltidsgraf" title=""/></div>\n'
		return l

	def constructChartURL(self):
		url = 'http://chart.apis.google.com/chart'
		url += '?cht=p3' # Chart type
		url += '&chs=600x240' # Chart size
		balances = self.e.get_balances()
		users = sorted(balances.iteritems(), key=lambda (k,v):v['totalcount'], reverse=True)
		cmap = self.e.get_colours()
		values = []
		colours = []
		aliases = []
		for u in users:
			alias = u[0]
			data = u[1]
			aliases.append(alias)
			values.append(data['totalcount'])
			colours.append(self.e.get_colour(alias)[1:])
		
		url += '&chd=t:%s' % ",".join([str(s) for s in values]) # 52,47,46,47,117.5,191.5,86.5
		url += '&chds=0,%s' % str(max(values)) # Scale values
		url += '&chl=%s' % "|".join(aliases) # Aliases
		url += '&chco=%s' % "|".join(colours) # Colour spec
		return url	
	
	def wishes(self):
		wl = self.e.get_wishes()
		l = '<h2>Indkøbsønsker</h2>\n'
		l += '<ul>\n'
		for w in wl:
			l += '<li>%s</li>\n' % w
		l += '</ul>\n'
		return l
	
	def index(self):
		o = '<h1>EggsML</h1>'
		o += '<a href="./graph_timeline.py">Se graf over deltagelse til Eggs</a>.'
		o += self.aliases()
		o += self.balances()
		o += self.wishes()
		o += self.purchases()
		self.output = o
		return o
	
	def currency(self, i):
		i = str(round(i,2)).split('.')
		if len(i[1])==1:
			i[1] = i[1]+'0'
		return '%s,<span class="ears">%s</span>' % (i[0], i[1])
	
	def pointer(self, i):
		return str(i).replace('.', ',')
	
	def __init__(self):
		self.e = eggsml()
		self.e.parse('slashdotfrokost')
