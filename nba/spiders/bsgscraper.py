import scrapy
import csv
from bs4 import BeautifulSoup as bs

teamlist=('gsw','lac','lal','pho','sac','dal','hou','mem','nor','sas','den','min','okc','por','uth','bos','brk','nyk','phi','tor','chi','cle','det','ind','mil','atl','cha','mia','orl','was')
years=[]
for year in range(2020,2022): #exclusive
    years.append("/"+str(year))
urlstoscrape=[]
for team in teamlist:
    for year in years:
        urlstoscrape.append("https://www.boxscoregeeks.com/teams/"+team+year)

class BSGSpider(scrapy.Spider):
    name = "bsg"

    def start_requests(self):
        urls = urlstoscrape
        for url in urls:
            yield scrapy.Request(url=url, callback=self.parse)

    def parse(self, response):
        page = '_'.join([response.url.split("/")[-1],response.url.split("/")[-2]])
        filename = 'nba/bsg_team_pages/NewTeamPagesGoHereToAvoidAccidentalOverwrite/%s.html' % page
        with open(filename, 'wb') as f:
            f.write(response.body)
        # table = response.xpath('//*[@class="stats-table player-stats-table"]//tbody')
        # rows = table.xpath('//tr')
        # soup = bs(response.body, 'html.parser')
        # pretty=soup.prettify()
        # a=1
        # tibbles = {}
        # for tibble in soup.find_all('table'):#{, 'class': 'stats-table player-stats-table'}):
        #     tibbles['data'+str(a)]=[]
        #     tibble_body = tibble.find('tbody')
        #     rows = tibble_body.find_all('tr')
        #     for row in rows:
        #         cols = row.find_all('td')
        #         cols = [ele.text.strip() for ele in cols]
        #         tibbles['data'+str(a)].append([ele for ele in cols if ele])
        # a=a+1
        # with open(page+'1.csv', 'w') as file:
        #     f = csv.writer(file,delimiter=',',quoting=3,quotechar='',escapechar='')
        #     f.writerow(['this is the header','whee'])
        #     for i in range(3):
        #         try:
        #             for row in tibbles[data+str(i)]:
        #                 f.writerow(row)
        #         except:
        #             pass

        # with open(page + '.txt', 'wb') as f:
        #     f.write(pretty.encode('utf-8'))        
        # with open(page+'.csv', 'w') as file:
        #     f = csv.writer(file,delimiter=',',quoting=3,quotechar='',escapechar='')
        #     f.writerow(['this is the header','whee'])
        #     for i in range(25):
        #         try:
        #             f.writerow([rows[i].xpath('td//text()')[1].extract()])
        #         except KeyError:
        #             pass
        # self.log('Saved file %s' % filename)

