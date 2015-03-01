# -*- encoding: utf-8 -*-
# vim: noexpandtab:ts=4 :
import time, re
import eggsml_math
from datetime import date
import random
import numpy

class eggsml:

	aliases = []
	count = {}
	wishes = []
	purchases = []
	dates = []
	uniquedates_dict = {}
	uniquedates = None
	dayprices = {}
	colours = {}
	content = ''
	users = []
	startdate = None
	enddate = None
	nodays = None

	def get_alias_rand(self,eggname):
		for aliaslist in self.aliases: 
			if eggname==aliaslist[0]:
				return random.choice(aliaslist)
		return None

	def set_startdate(self, somedate):
		self.startdate = somedate
	
	def set_enddate(self, somedate):
		if somedate==None:
			self.enddate = self.get_last_date()
		else:
			self.enddate = somedate
	
	def get_aliases(self):
		return self.aliases
	
	def get_colours(self):
		return self.colours
	
        def get_colour(self, alias):
            try:
                    colour = self.colours[alias]
                    return colour
            except:
                    return '#%02x%02x%02x' % (random.randint(0,200), random.randint(0,200), random.randint(0,200))

	def get_purchases(self):
		return self.purchases
	
	def get_wishes(self):
		return self.wishes
		
	def get_dates(self):
		return self.dates

	def datesort(self, x, y):
		return (-1 if x['date']<y['date'] else 1)

	def get_unique_dates(self):
		if self.uniquedates == None:
			self.uniquedates = [
				{ 'date' : d, 'users': [
					{ 'user' : u, 'amount' : a } for (u, a) in us.iteritems()
				] } for (d, us) in self.uniquedates_dict.iteritems()
			]
			self.uniquedates.sort(self.datesort)
		return self.uniquedates
	
	def get_count(self):
		return self.count
	
	def get_users(self):
		if len(self.users) > 0:
			return self.users
		u = []
		for d in self.dates:
			for t in d['users']:
				if t['user'] not in u:
					u.append(t['user'])
		return u

	def get_amount_of_days(self):
		if self.nodays == None:
			start = self.get_first_date()
			start = int(date(start.year, start.month, start.day).strftime("%s"))
			end = self.get_last_date()
			end = int(date(end.year, end.month, end.day).strftime("%s"))
			x = int("%s" % start)
			days = 0
			while x <= end:
				x += 60*60*24
				days += 1
			self.nodays = days
		return self.nodays
	
	def get_first_date(self):
		if self.startdate!=None:
			return self.startdate
		t = self.dates
		t.sort(self.datesort)
		for d in t:
			if d['date'].year==1970:
				continue
			self.startdate = d['date']
			return d['date']
	
	def get_last_date(self):
		if self.enddate!=None:
			return self.enddate
		t = self.dates[:]
		t.sort(self.datesort)
		t.reverse()
		for d in t:
			if d['date'].year==1970:
				continue
			self.enddate = d['date']
			return d['date']
	
	def get_user_timeline(self, user):
		u = self.get_alias(user)
		tmp = []
		dates = self.dates[:]
		dates.sort(self.datesort)
		predate_count = 0.0
		for d in dates:
			if d['date'] < self.get_first_date():
				for t in d['users']:
					if t['user'] == u:
						predate_count += t['amount']
			elif self.get_last_date() < d['date']:
				pass
			else:
				if predate_count>0.0:
					tmp.append({'date' : date(1970, 1, 1), 'amount' : predate_count})
					predate_count = 0.0
				for t in d['users']:
					if t['user'] == u:
						tmp.append({'date' : d['date'], 'amount' : t['amount']})
		nochange = False
		if predate_count>0.0:
			tmp.append({'date' : date(1970, 1, 1), 'amount' : predate_count})
			predate_count = 0.0
			nochange = True
		ntmp = []
		curdate = self.previous_date(self.get_first_date())
		#curdate = self.get_first_date()
		tmp.sort(self.datesort)
		useless = True
		for t in tmp:
			ntmp += self.add_empty_timeline_rows(curdate, t['date'])
			ntmp.append({'date' : t['date'], 'amount' : t['amount']})
			#print user,t
			curdate = t['date']
			if t['amount']>0.0:
				useless = False
		if nochange:
			ntmp += self.add_empty_timeline_rows(self.get_first_date(), self.get_last_date(), False, True)
		else:
			ntmp += self.add_empty_timeline_rows(curdate, self.get_last_date(), False)
		if ntmp[0]['date'].year!=1970:
			ntmp = [{'date' : date(1970, 1, 1), 'amount' : 0}] + ntmp
		if useless:
			return None
		#print ntmp
		return ntmp
	
	def date_after(self, somedate):
		t = time.gmtime(time.mktime(somedate.timetuple()) + 60*60*24)
		return date(t[0], t[1], t[2])
		
	
	def previous_date(self, somedate):
		t = time.gmtime(time.mktime(somedate.timetuple()) - 60*60*24)
		return date(t[0], t[1], t[2])
	
	def add_empty_timeline_rows(self, predate, nowdate, addone=True, withend=False):
		if predate.year==1970:
			predate = self.previous_date(self.get_first_date())
		t0 = time.mktime(predate.timetuple()) + (60*60*24 if addone else 0)
		t1 = time.mktime(nowdate.timetuple())
		tmp = []
		while (withend and t0<=t1) or (not withend and t0<t1):
			t = time.gmtime(t0)
			tmp.append({'date' : date(t[0], t[1], t[2]), 'amount' : 0})
			t0 += 60*60*24
		return tmp
	
	def z(self, i):
		if i<10:
			return "0%s" % i
		else:
			return str(i)
	
	def get_alias(self, alias, maxlength=None):
		if maxlength == None:
			if alias not in self.aliasmap: return None
			return self.aliasmap[alias]

		found = False
		aliases = None
		for row in self.aliases:
			for a in row:
				if a==alias:
					found = True
					break
			if found:
				aliases = row
				break

		shortest = None
		for a in aliases:
			if len(shortest)>len(a) or shortest == None:
				shortest = a
			if len(a)>maxlength:
				continue
			else:
				return a
		return shortest
	
	# calculate the meal price for each day
	# returns a list of dictionaries containing date and price
	def get_day_prices(self):
		if len(self.dayprices) > 0:
			return self.dayprices
		dates = self.dates[:]
		dates.sort(self.datesort)
		purchases = self.purchases[:]
		purchases.sort(self.datesort)
		
		# arrange all meals in one array
		# add each purchase in the place of the first-coming meal
		meals = []
		purchase_idx = 0
		i = 0
		for d in dates:
			skipfirst = False
			purchase = 0;
			# add purchase if one has happened since last date
			while (purchase_idx < len(purchases) 
					and d['date'] >= purchases[purchase_idx]['date']):
				purchase = purchase + purchases[purchase_idx]['amount']
				purchase_idx = purchase_idx + 1
			if purchase != 0:
				meals.append(purchase)
				i = i+1
				skipfirst = True
			# add half meals
			half_meals = self.get_half_meals_in_date(d)
			for x in range(0, half_meals):  
				# skip first meal if a purchase has been added
				if skipfirst:
					skipfirst = False
					continue
				meals.append(0)
				i = i+1
		

		# convolve with gaussian kernel to smooth out the expenses 
		filter_size = 700
		span = filter_size/2
		head = meals[:span]
		tail = meals[-span:]
		head.reverse()
		tail.reverse()
		price_meal = head+meals+tail
#		linear = [1./filter_size]*filter_size
#		price_meal = eggsml_math.conv(price_meal, linear, shape='same')
		gauss = eggsml_math.make_gaussian(filter_size, 150)
		price_meal = numpy.convolve(price_meal, gauss, mode='same')
		price_meal = price_meal[span:-span]

		# map the meal prices to price per day
		dayprices = {}
		i = 0
		for d in dates:
			half_meals = self.get_half_meals_in_date(d)
			price = 0
			for x in range(0, half_meals):
				price = price + price_meal[i]				
				i = i+1
			price = price/half_meals*2
			dayprices.update({d['date'] : price})
		self.dayprices = dayprices
		return self.dayprices
	
	def get_half_meals_in_date(self, d):
		half_meal_count = 0
		for u in d['users']:
			half_meal_count += int(u['amount']*2)
		return half_meal_count	
	
        def get_userinfo(self):
                prices = self.get_day_prices()
                # Initialize balances to the users expenses
                info = {}
                
                for u in self.get_users():
                        info[u] = {'balance' : 0.0,
                                   'paid' : 0.0,
                                   'eggscount' : 0,
                                   'lasteggs' : None}

                for p in self.get_purchases():
                        if p['alias'] in info: # HACK because
                                               # get_users fails for
                                               # people who've never
                                               # attended a lunch.
                                info[p['alias']]['paid'] += float(p['amount'])
                                info[p['alias']]['balance'] += float(p['amount'])
                                                
                for d in self.get_dates():
                        price = prices[d['date']]
                        for u in d['users']:
                                info[u['user']]['balance'] -= price * u['amount']
                                info[u['user']]['eggscount'] +=  u['amount']

                for useralias in info.keys():
                  dates = self.get_dates()
                  for d in reversed(dates):
                    for u in d['users']:
                      if useralias == u['user'] and info[useralias]['lasteggs'] == None:
                          info[useralias]['lasteggs'] = (date.today() - d['date']).days
                          break
                                                      
                return info

        def get_latest_lunch_date(self, user):
                dates = self.get_dates()
                today = date.today()
                for d in reversed(dates):
                  for u in d['users']:
                    if user == u['user']:
                      diff = today - d['date']
                      return diff.days
        	
	def get_average_price(self, enddate=None):
		total = 0.0
		users = {}
		purchases = self.purchases
		purchases.sort(self.datesort)
		for p in purchases:
			if enddate!=None and enddate < p['date']:
				break
			total += p['amount']
			try:
				users[p['alias']]['paid'] += float(p['amount'])
			except:
				users.update({p['alias'] : {'paid' : float(p['amount']), 'count' : 0}})
			if enddate!=None:
				if p['date']>enddate:
					break
		totalcount = 0.0
		dates = self.dates[:]
		dates.sort(self.datesort)
		for d in dates:
			#print enddate,d['date']
			if enddate!=None and enddate < d['date']:
				break
			for u in d['users']:
				#print u
				try:
					users[u['user']]['count'] += u['amount']
				except:
					users.update({u['user'] : {'paid' : 0.0, 'count' : u['amount']}})
				totalcount += u['amount']
			if enddate!=None:
				if d['date'] > enddate:
					break
		avg = total/totalcount
		return avg
	
	def before_date(self, testdate, somedate):
		if testdate==None or somedate==None:
			return False
		return testdate<somedate
	
	def is_date_before_or_same(self, futuredate, nowdate):
		if futuredate==nowdate:
			return True
		t0 = time.mktime(time.strptime(futuredate, '%Y-%m-%d'))
		t1 = time.mktime(time.strptime(nowdate, '%Y-%m-%d'))
		return t0<t1
	
	def check_count_conflict(self):
		counttotal = 0.0
		for c in self.count:
			counttotal += self.count[c]
		datetotal = 0.0
		for d in self.dates:
			for u in d['users']:
				datetotal += u['amount']
	
	def add_alias(self, line):
		if line.strip()=='':
			return
		aliases = line.strip().split(',')
		tmp = []
		baseAlias = aliases[0].strip()
		for a in aliases:
			sa = a.strip()
			tmp.append(sa)
			self.aliasmap[sa] = baseAlias
		self.aliases.append(tmp)
		self.count.update({tmp[0] : 0})
	
	def add_count(self, line):
		if line.strip()=='':
			return
		tmp = line.strip().split(':')
		name = self.get_alias(tmp[0].strip())
		countstr = tmp[1].strip()
		count = countstr.count('C')*100.0 + countstr.count('L')*50.0 + countstr.count('X')*10.0 + countstr.count('|')*1.0 + countstr.count("'")*0.5
		self.count[name] = count
		
	def add_wish(self, line):
		if line.strip()=='':
			return
		tmp = line.strip()
		if tmp == 'eggscetera':
			return True
		self.wishes.append(tmp)
	
	def add_purchase(self, line):
		tmp = line.strip()
		if tmp=='':
			return
		tmp = tmp.split(',')
		t = tmp[0].strip().split('-')
		tmp = tmp[1].split(':')
		name = self.get_alias(tmp[0].strip())
		amount = tmp[1].strip()
		f = float(amount)
		tmp = {'date' : date(int(t[0]), int(t[1]), int(t[2])), 'amount' : f, 'alias' : name}
		self.purchases.append(tmp)
	
	def add_date(self, line):
		tmp = line.strip()
		if tmp=='':
			return
		tmp = tmp.split(',', 1)
		thedate = tmp[0].strip()
		users = tmp[1].strip().split(',')
		u = []
		for usr in users:
			amount = 1.0
			name = usr
			if usr.count('(')>0:
				data = usr.split('(')
				name = data[0]
				amount = float(data[1].strip('*').strip(')'))
			if name.strip()=='':
				continue
			u.append({'user' : self.get_alias(name.strip()), 'amount' : amount})
		t= thedate.split("-")
		day = date(int(t[0]), int(t[1]), int(t[2]))
		tmp = {'date' : day, 'users' : u}
		self.dates.append(tmp)

		if day not in self.uniquedates_dict:
			self.uniquedates_dict[day] = {}
		daydata = self.uniquedates_dict[day]
		for usr in u:
			uname = usr['user']
			if uname not in daydata: daydata[uname] = 0
			daydata[uname] += usr['amount']
		


	def add_colour(self, line):
		tmp = line.strip()
		if tmp=='':
			return
		tmp = tmp.split(':')
		user = self.get_alias(tmp[0].strip())
		colour = tmp[1].strip()
		self.colours.update({user : colour})
	
	def fix_float(self, f):
		return round(f, 2)

	def parse(self, filename):
		self.aliases = []
		self.aliasmap = {}
		self.count = {}
		self.wishes = []
		self.purchases = []
		self.dates = []
		self.uniquedates_dict = {}
		self.uniquedates = None
		self.dayprices = {}
		self.colours = {}
		self.content = ''
		self.users = []
		self.startdate = None
		self.enddate = None
		self.nodays = None
		r = re.compile(r"#.*?$", re.MULTILINE | re.UNICODE)
		self.content = open(filename).read()
		block = ''
		for l in self.content.splitlines():
			if l.strip()[0:2]=='::':
				t = l.strip().strip('::')
				if t=='ALIAS':
					block = 'alias'
				elif t=='MÅLTIDER':
					block = 'count'
				elif t=='INDKØBSØNSKER':
					block = 'wishes'
				elif t=='INDKØB':
					block = 'purchase'
				elif t=='MÅLTIDSDATA':
					block = 'dates'
				elif t=='FARVER':
					block = 'colours'
				else:
					block = ''					 
			else:
				if block!='colours':
					l = r.sub('', l)
				if block=='alias':
					self.add_alias(l)
				elif block=='count':
					self.add_count(l)
				elif block=='wishes':
					self.add_wish(l)
				elif block=='purchase':
					self.add_purchase(l)
				elif block=='dates':
					self.add_date(l)
				elif block=='colours':
					self.add_colour(l)
				else:
					pass

		self.dates.sort(self.datesort)
		self.users = self.get_users()
