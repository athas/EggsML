#!/usr/bin/env python
# -*- encoding: utf-8 -*-

from eggsml import eggsml
import random
import time
import math
from datetime import date, datetime, timedelta
from server import server

class graph:
	e = None
	svg = ''
	height = 0 #set by __init__
	width = 0 #set by __init__
	gwidth = None
	gheight = 560
	padding = [65, 50, 20, 50]
	weekwidth = 70
	daywidth = 12
	userweeks = {}
	weeks = []
	days = []
	prices = {}
	user_scores = {}
	week_scores = {}
	dayscores = {}
	colours = None
	yleftaxisinc = 10
	yrightaxisinc = 20
	highscore = 0
	startdate = None
	enddate = None
	users = None
	maxprice = 25
	timeset = False
	singleday = False
	namelabellocations = []
	
	def l(self, i, c):
		if i==0:
			return 0
		if i==1:
			return 0
		return math.log(float(i))*c

	def compare_positions(self, pos1, pos2):
		# compare if x2 is less than the other's x1
		# if so, then they cannot overlap
		if pos1[2] < pos2[0] or pos2[2] < pos1[0]:
			return False
		# compare if y2 is less than the other's y1
		# if so, then they cannot overlap
		if pos1[3] < pos2[1] or pos2[3] < pos1[1]:
			return False
		# if neither of these are the cases,
		# then they should be overlapping
		return True
		

	def taken_name_label_position(self, position):
		for nl in self.namelabellocations:
			if self.compare_positions(nl, position):
				return True
		return False

	def create_position(self, right, x, y, width, height, textheight):
		x1 = (x + 25 if right else x - width)
		x2 = (x + width if right else x - 25)
		y1 = y - height - textheight
		y2 = y - height
		return (x1, y1, x2, y2)

	def get_name_label_position(self, x, y, width):
		textheight = 12
		right = False
		if x-width-self.padding[3]<0:
			right = True
		if self.namelabellocations == []:
			height = 40
			position = self.create_position(right, x, y, width, height, textheight)
		else:
			testheight = 10
			position = self.create_position(right, x, y, width, testheight, textheight)
			while self.taken_name_label_position(position):
				testheight += 10
				position = self.create_position(right, x, y, width, testheight, textheight)
		x1 = position[0]
		x2 = position[2]
		y1 = position[1]
		y2 = position[3]
		self.namelabellocations.append((x1, y1, x2, y2))
		if right:
			return (x2, x1, x1, y2)
		else:
			return (x1, x2, x1, y2)
	
	def userbar(self, user):
		ww = self.weekwidth
		cw = self.daywidth
		timeline = self.e.get_user_timeline(user)
		if timeline==None:
			return None
		line = ''
		weeks = []
		x = self.padding[3]
		h = self.height-self.padding[2]
		lineinc = self.yrightaxisinc
		curweek = 0
		curweekstart = ''
		predate = '1970-01-01'
		y = h
		total = 0.0
		svg_name = None
		scores = {}
		curscore = 0.0
		for d in timeline:
			if d['amount'] > 0:
				try:
					self.dayscores[d['date']] += 1
				except:
					self.dayscores.update({d['date'] : 1})
			curscore += d['amount']
			if curscore > 0.0:
				y = h - self.l(curscore+1, lineinc)
			total += d['amount']
			if d['date'] < self.startdate:
				continue
			if d['date'].year==1970:
				# special case
				line += '%s,%s ' % (x, y)
				weeks.append({'first' : d['amount']})
			else:
				# regular case
				line += '%s,%s ' % (x, y)
				if curweekstart=='':
					curweekstart = self.week_start(d['date'])
				if self.is_new_week(d['date'], curweekstart):
					weeks.append({curweekstart : curweek})
					curweek = d['amount']
					curweekstart = self.week_start(d['date'])
				else:
					curweek += d['amount']
				if d['amount']>0.0 and svg_name==None:
					nwidth = len(user)*12+15
					(nx1, nx2, ntx, ny) = self.get_name_label_position(x, y, nwidth)
					svg_name = '<rect x="%s" y="%s" width="%s" height="%s" style="fill: %s; opacity: 0.0;" />\n<text x="%s" y="%s" style="fill: %s;">%s</text>\n<polyline points="%s,%s %s,%s %s,%s" style="stroke: %s; stroke-dasharray: 3, 3;" />\n' % (ntx, ny-15, nwidth-25, 15, '#444', ntx, ny, self.get_colour(user), user, nx1, ny, nx2, ny, x, y, self.get_colour(user))
			scores.update({d['date'].isoformat() : curscore})
			if d['date'] not in self.days:
				self.days.append(d['date'])
			x += cw
		if d['date']!=self.e.get_last_date():
			scores.update({self.e.get_last_date().isoformat() : curscore})
		if svg_name==None:
			svg_name = ''
		self.user_scores.update({self.id_fix(user) : scores})
		weeks.append({curweekstart : curweek})
		self.userweeks.update({user : weeks})
		svg_line = '<polyline points="%s" style="stroke:%s" id="line:%s" />\n<!--<text x="0" y="0" id="user-%s-score-label" class="line-label"> </text>-->\n' % (line, self.get_colour(user), self.id_fix(user), self.id_fix(user))
		svg_text = '<text x="%s" y="%s" style="fill:%s">%s</text>\n' % (x-cw+2, y+4, self.get_colour(user), self.decimal(total)) + '<!-- %s || %s -->' % (timeline, self.e.get_first_date())
		if date(1970, 1, 1) not in self.days:
			self.days.append(date(1970, 1, 1))
		return (svg_line, svg_text, svg_name)
	
	def test(self, timeline):
		tmp = []
		for t in timeline:
			tmp.append(t['date'])
		return tmp
	
	def id_fix(self, string):
		if string==None:
			return ''
		return string.replace('æ', 'ae').replace('ø', 'oe').replace('å', 'aa')
	
	def is_new_week(self, curdate, weekdate):
		t0 = curdate.timetuple()
		t1 = weekdate.timetuple()
		w0 = time.strftime("%W", t0)
		w1 = time.strftime("%W", t1)
		return w0!=w1
	
	def week_start(self, hdate):
		t0 = hdate.timetuple()
		while date(t0[0], t0[1], t0[2]).weekday()>0:
			t = time.mktime(t0)
			t0 = time.localtime(t-60*60*24)
		tmp = date(t0[0], t0[1], t0[2])
		if tmp not in self.weeks:
			self.weeks.append(tmp)
		return tmp
	
	def svg_users(self):
		if self.users == None:
			users = self.e.get_users()
		else:
			users = self.users
		svg_line = '<g id="user-lines">\n'
		svg_text = '<g id="scores-labels">\n'
		svg_name = '<g id="names-labels">\n'
		for u in users:
			t = self.userbar(u)
			if t==None:
				continue
			svg_line += '%s\n' % t[0]
			svg_text += '%s\n' % t[1]
			svg_name += '%s\n' % t[2]
		svg_line += '</g>\n'
		svg_text += '</g>\n'
		svg_name += '</g>\n'
		return (svg_line, svg_text, svg_name)
	
	def svg_weeks(self):
		if self.weeks == []:
			return
		x = self.padding[3]+self.daywidth/2.0
		y = self.padding[0]
		h = self.gheight
		w = self.weekwidth
		columninc = self.yrightaxisinc
		svg_back = ''
		svg_cols = '<g id="week-cols">\n'
		svg_lbls = '<g id="week-labels">\n'
		colour = '#c3c3c3'
		user_col_labels = None
		week_scores = {}
		for week in self.weeks:
			if self.before_date(week, self.e.get_first_date()):
				# assume it is first date
				offset = self.date_offset(week, self.e.get_first_date())
				w = self.daywidth*(7-offset)
			else:
				if abs(self.date_offset(week, self.e.get_last_date()))<7:
					# assume last week
					w = self.daywidth*abs(self.date_offset(week, self.e.get_last_date()))+self.daywidth/2.0
				else:
					w = self.weekwidth
			svg_back += '<rect x="%s" y="%s" width="%s" height="%s" style="fill:%s" />\n' % (x, y, w, h, colour)
			tx = x+w/2-2*3
			ty = y+self.gheight+10
			svg_lbls += '<text x="%s" y="%s">%s</text>\n' % (tx, ty, self.week_number(week))
			if colour == '#c3c3c3':
				colour = '#f3f3f3'
			else:
				colour = '#c3c3c3'
			count = 0
			thisusers = []
			for user in self.userweeks:
				for thisweek in self.userweeks[user]:
					try:
						t = thisweek[week]
						count += 1
						thisusers.append([user, t])
					except:
						continue
			if count>0:
				week_scores.update({week.isoformat() : {}})
				columnwidth = w/count
				cx = x
				ctop = self.padding[0] + self.gheight
				svg_cols += '<g id="week-cols:%s">\n' % week
				addlabels = False
				if user_col_labels == None:
					addlabels = True
					user_col_labels = '<g id="week-cols-labels">\n'
				for user in thisusers:
					ch = columninc*user[1]
					cy = ctop - ch
					svg_cols += '<rect x="%s" y="%s" width="%s" height="%s" style="fill:%s" id="week:%s:col:%s" onmouseover="show_col_score(this);" onmouseout="hide_col_score(this);" />\n' % (cx, cy, columnwidth, ch, self.get_colour(user[0]), week, self.id_fix(user[0]))
					cx += columnwidth
					week_scores[week.isoformat()].update({self.id_fix(user[0]) : user[1]})
					if addlabels:
						user_col_labels += '<text x="0" y="0" id="week-col-label-%s"> </text>\n' % (self.id_fix(user[0]))
				svg_cols += '</g>\n'
			x += w
		user_col_labels += '</g>\n'
		svg_back += ''
		svg_cols += user_col_labels
		svg_cols += '</g>\n'
		svg_lbls += '</g>\n'
		self.week_scores = week_scores
		return (svg_back, svg_cols, svg_lbls)
	
	def week_number(self, somedate):
		return int(time.strftime("%W", somedate.timetuple()))
	
	def before_date(self, testdate, somedate):
		return testdate<somedate
		
	def date_offset(self, firstdate, seconddate):
		t0 = time.mktime(firstdate.timetuple())/(60*60*24)
		t1 = time.mktime(seconddate.timetuple())/(60*60*24)
		return int(t1-t0)
	
	def svg_days(self):
		if self.days==[]:
			return
		svg_back = '<g id="days-back">\n'
		svg_lbls = '<g id="days-labels">\n'
		svg_price = '<g id="average-price">\n'
		svg_bars = '<g id="days-bars">\n'
		x = self.padding[3]+self.daywidth/2.0
		y = self.padding[0]
		w = self.daywidth
		h = self.gheight
		lx = self.padding[3]+self.daywidth
		ly = self.padding[0]
		colour = '#ffffff'
		price_coords = ''
		dayprices = self.e.get_day_prices()
		prices = {}
		py = h+y
		self.days.sort()
		#print dayprices
		curprice = 0
		for d in self.days:
			if d.year==1970:
				svg_back += '<rect x="%s" y="%s" width="%s" height="%s" id="back:%s" style="fill: %s; opacity: 0.5;" />\n' % (self.padding[3], y, w/2.0, h, d, '#f30000')
				svg_lbls += '<text x="%s" y="%s" transform="rotate(315, %s, %s)">%s</text>\n' % (lx-self.daywidth, ly, lx-self.daywidth, ly, "Dawn of time")
				continue
			if d==self.e.get_last_date():
				w = w/2.0
			try:
				prices.update({d.isoformat(): dayprices[d]})
				curprice = dayprices[d]
			except:
				prices.update({d.isoformat(): curprice})
			svg_back += '<rect x="%s" y="%s" width="%s" height="%s" id="back:%s" style="fill: %s;" />\n' % (x, y, w, h, d, colour)
			svg_lbls += '<text x="%s" y="%s" transform="rotate(315, %s, %s)">%s</text>\n' % (lx, ly, lx, ly, d.isoformat())
			try:
				ds = self.dayscores[d]
			except:
				ds = 0
			svg_bars += '<rect x="%s" y="%s" width="%s" height="%s" />\n' % (x, self.padding[0]+self.gheight-self.l(ds, self.yrightaxisinc), w, self.l(ds, self.yrightaxisinc))
			cy = py-curprice*self.yleftaxisinc
			price_coords += '%s,%s ' % (lx, cy)
			if colour=='#e3e3f3':
				colour = '#ffffff'
			else:
				colour = '#e3e3f3'
			lx += w
			x += w
			if d==self.e.get_last_date():
				break
		svg_price += '<text id="date-price-label" class="line-label" x="0" y="0"> </text>\n'
		svg_price += '<polyline points="%s" id="average_price_line" />\n<!-- %s -->\n' % (price_coords, prices)
		self.prices = prices
		svg_back += '</g>\n'
		svg_lbls += '</g>\n'
		svg_price += '</g>\n'
		svg_bars += '</g>\n'
		return (svg_back, svg_lbls, svg_price, svg_bars)
	
	def svg_labels(self):
		svg = '<g id="amount-labels">\n'
		x1 = self.padding[3]/2.0-5
		x2 = self.padding[3]+self.gwidth+8
		y = self.padding[0]+self.gheight
		j = 0
		while j <= self.highscore:
			ty = y
			if j > 0:
				ty = y - self.l(j+1, self.yrightaxisinc)
			svg += '<line x1="%s" y1="%s" x2="%s" y2="%s" />\n<text x="%s" y="%s">%s</text>\n' % (self.padding[3], ty, self.padding[3]+self.gwidth, ty, x2, ty+5, int(j))
			if j>0:
				j += 10**math.floor(math.log10(j))
			else:
				j += 1
		ty = y - self.l(j, self.yrightaxisinc)
		svg += '<line x1="%s" y1="%s" x2="%s" y2="%s" />\n<text x="%s" y="%s">%s</text>\n' % (self.padding[3], ty, self.padding[3]+self.gwidth, ty, x2, ty+5, int(j))
		for i in range(0, self.maxprice):
			y -= self.yleftaxisinc
			svg += '<text x="%s" y="%s">%s</text>\n' % (x1, y, i+1)
			
		svg += '</g>\n'
		return (svg,)
	
	def decimal(self, i):
		return str(float(i)).replace('.', ',')
	
	def average_price(self, somedate):
		return self.e.get_average_price(somedate)
		
	def get_colour(self, alias):
		if self.colours==None:
			self.colours = self.e.get_colours()
		try:
			colour = self.colours[alias]
			return colour
		except:
			self.colours.update({alias : 'rgb(%s, %s, %s)' % (random.randint(0,200), random.randint(0,200), random.randint(0,200))})
			return self.colours[alias]
	
	def make_svg(self):
		users = self.svg_users()
		weeks = self.svg_weeks()
		days = self.svg_days()
		lbls = self.svg_labels()
		svg = '''<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<svg
	xmlns:dc="http://purl.org/dc/elements/1.1/"
	xmlns:cc="http://creativecommons.org/ns#"
	xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
	xmlns:svg="http://www.w3.org/2000/svg"
	xmlns="http://www.w3.org/2000/svg"
	width="%s"
	height="%s"
	onload="init();">
<script type="text/javascript">
<![CDATA[
var prices = %s;
var users = %s;
var weeks = %s;
/*
  First is daywidth
  Second is priceline inc.
  Third is userscoreline inc.
*/
var specifics = [%s, %s, %s];
var svg = [%s, %s];
var padding = %s;

function init() {
	var g = document.getElementById("average_price_line");
	g.onmouseover = function(e) { show_price(e); };
	var g = document.getElementById("user-lines").getElementsByTagName('polyline');
	for(var i = 0; i < g.length; i++) {
		g[i].onmouseover = function(e) { show_user_score(e, this); };
	}
};

function show_price(e) {
	var date = find_date(e);
	var price = prices[date];
	var text = document.getElementById("date-price-label");
	text.firstChild.data = currency(price);
	var x = datex(date);
	var y = svg[1] - padding[2] - specifics[1]*price - 10;
	text.setAttribute('x', x);
	text.setAttribute('y', y);
	var nextdate = find_date(e, true);
	var a = angle(price, prices[nextdate], specifics[1], specifics[0]);
	text.setAttribute('transform', 'rotate('+a+','+x+','+y+')');
}

function angle(a, b, lineinc, width) {
	var diff = (a-b)*lineinc;
	var d = (180/Math.PI) * Math.atan(diff/width);
	return d;
}

function find_date(e, dateafter) {
	var g = document.getElementById("days-back");
	var backs = g.getElementsByTagName("rect");
	var x = e.pageX;
	var y = e.pageY;
	for(var i = 0; i < backs.length; i++) {
		var tempx = backs[i].getAttribute('x');
		if (tempx<x)
			continue;
		if ((tempx>x)/* && (x<tempx+backs[i].getAttribute('width'))*/) {
			if (dateafter)
				i++;
			var id = backs[i-1].id.split(':');
			var date = id[1];
			return date;
		}
	}
	var id = backs[i-1].id.split(':');
	var date = id[1];
	return date;	
}

function datex(date) {
	var d = document.getElementById("back:"+date);
	var x = d.getAttribute('x');
	var w = d.getAttribute('w');
	return x+(w/2);
}

function show_user_score(e, which) {
	var user = which.id.split(':')[1];
	var date = find_date(e);
	var points = users[user][date];
	var x = datex(date);
	var y = svg[1] - padding[2] - specifics[2]*points - 10;
	var text = document.getElementById("user-"+user+"-score-label");
	text.firstChild.data = decimal(points);
	text.setAttribute('x', x);
	text.setAttribute('y', y);
	var nextdate = find_date(e, true);
	var a = angle(points, users[user][nextdate], specifics[2], specifics[0]);
	text.setAttribute('transform', 'rotate('+a+','+x+','+y+')');
}

function currency(x) {
	var v = x + 0.0;
	var v = String(v).split('.');
	if(!v[1])
		b = '00';
	else {
		if (v[1].length==1)
			b = v[1] + '0';
		else
			b = v[1];
	}
	return v[0] + ',' + b;
}

function decimal(x) {
	var v = x + 0.0;
	return String(v).replace('.', ',');
}

function z(x) {
	var x = x;
	if (x < 10) {
		return "0" + x;
	} else {
		return "" + x;
	}
}

function show_col_score(el) {
	var p = el.parentNode;
	var g = p.getElementsByTagName('rect');
	var week = p.id.split(':')[1];
	var weekdata = weeks[week];
	for(var i = 0; i < g.length; i++ ) {
		var user = g[i].id.split(':')[3];
		var points = weekdata[user];
		if(!points || points=='NaN')
			points = '0.0';
		var text = document.getElementById('week-col-label-'+user);
		var x = parseFloat(g[i].getAttribute('x')) + parseFloat(g[i].getAttribute('width'))/2.0 + 5.0;
		var y = g[i].getAttribute('y');
		text.setAttribute('x', x);
		text.setAttribute('y', y);
		text.setAttribute('transform', 'rotate(270,'+x+','+y+')');
		text.firstChild.data = user + ': ' + decimal(points);
	}
}

function hide_col_score(el) {

}

]]>
</script>
<style text="text/css">
text {
	font-family: helvetica, verdana, tahoma, sans;
}
#labels text {
	fill: #ffffff;
	stroke-width: 1px;
}
#week-labels text {
	font-size: 10px;
}
#days-labels text {
	font-size: 9px;
}
#days-back rect {
	opacity: 0.35;
}
#amount-labels text,
#scores-labels text {
	font-size: 12px;
}
#names-labels text {
	font-size: 14px;
}
#amount-labels line {
	stroke-width: 1.5px;
	stroke: #505050;
	opacity: 0.5;
}
.line-label {
	font-size: 14px;
	stroke: none;
}
polyline {
	stroke-width: 2px;
	fill: none;
	stroke-linejoin: round;
	stroke-linecap: round;
}
#average-price {
	stroke: #000000;
	stroke-dasharray: 9, 5;
	stroke-width: 1.5px;
}
#days-bars rect {
	fill: #00a;
	opacity: 0.25;
}
</style>
''' % (self.width, self.height, self.prices, self.user_scores, self.week_scores,
		self.daywidth, self.yrightaxisinc, self.yleftaxisinc, self.width,
		self.height, self.padding)
		svg += weeks[0]
		svg += days[0]
		#svg += weeks[1] # no columns, please
		svg += days[1]
		svg += days[3]
		svg += weeks[2]
		svg += lbls[0]
		svg += users[0]
		svg += users[1]
		svg += users[2]
		svg += days[2]
		svg += '''
</svg>'''
		self.svg = svg
		
	def weeks_earlier(self, somedate, weeks):
		t0 = time.localtime(time.mktime(somedate.timetuple()) - weeks*7*24*60*60)
		tmp = self.week_start(date(t0[0], t0[1], t0[2]))
		return tmp
	
	def handle_input(self):
		s = server()
		startdate = s.get_get_data('startdate')
		#startdate = '2009-04-28'
		try:
			ts = time.strptime(startdate, '%Y-%m-%d')
			if time.time() < time.mktime(ts):
				startdate = None
			t = startdate.split('-')
			startdate = date(int(t[0]), int(t[1]), int(t[2]))
		except (AttributeError, ValueError):
			startdate = None
		self.startdate = startdate
		enddate = s.get_get_data('enddate')
		#enddate = '2009-05-03'
		try:
			te = time.strptime(enddate, '%Y-%m-%d')
			if startdate!=None and time.mktime(ts) > time.mktime(te):
				enddate = None
			if time.mktime(te)>time.time():
				enddate = None
			t = enddate.split('-')
			enddate = date(int(t[0]), int(t[1]), int(t[2]))
		except (AttributeError, ValueError):
			enddate = None
		self.enddate = enddate
		userstr = s.get_get_data('users')
		users = []
		for u in userstr.split(','):
			t = self.e.get_alias(u)
			if t!=None:
				users.append(t)
		if users==[]:
			users = None
		self.users = users
		widthstr = s.get_get_data('width')
		try:
			self.gwidth = int(widthstr)
		except ValueError:
			self.gwidth = None
		daywidthstr = s.get_get_data('daywidth')
		try:
			self.daywidth = int(daywidthstr)
		except ValueError:
			pass
		sincedawnstr = s.get_get_data('sincedawn')
		if sincedawnstr!=None and sincedawnstr!='':
			self.startdate = self.e.get_first_date()
		singleday = s.get_get_data('singleday')
		try:
			te = time.strptime(singleday, '%Y-%m-%d')
			t = singleday.split('-')
			singleday = date(int(t[0]), int(t[1]), int(t[2]))
		except (AttributeError, ValueError):
			singleday = None
		if singleday != None and singleday != '':
			self.startdate = singleday - timedelta(1)
			self.enddate = singleday + timedelta(1)
			self.singleday = True

	def get_dayamount(self):
		if not self.timeset:
			self.e.set_enddate(self.enddate)
			if self.startdate==None:
				d = self.e.get_last_date()
				self.startdate = self.weeks_earlier(d, 8)
			self.e.set_startdate(self.startdate)
			self.timeset = True
		return self.e.get_amount_of_days()

	def __init__(self):
		self.e = eggsml()
		self.e.parse('slashdotfrokost')
		self.handle_input()
		self.nodays = self.get_dayamount()
		if self.gwidth == None:
			self.gwidth = self.daywidth*self.nodays
		self.width = self.padding[1] + self.padding[3] + self.gwidth
		self.height = self.padding[0] + self.padding[2] + self.gheight
		count = self.e.get_count()
		highest = 0
		for c in count:
			if count[c]>highest:
				highest = count[c]
				u = c
		self.yleftaxisinc = self.gheight/self.maxprice
		self.yrightaxisinc =  self.gheight/math.log(highest)*0.9
		self.highscore = highest
		cdates = float(len(self.e.get_user_timeline(u)))-1.0
		#print cdates
		self.daywidth = self.gwidth/cdates
		self.weekwidth = self.daywidth*7.0
		self.make_svg()
		#return
		print "Content-type: image/svg+xml; charset=UTF-8\n"
		print self.svg

graph()
