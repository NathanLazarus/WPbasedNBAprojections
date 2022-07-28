import scrapy
import csv

class BSGSpider(scrapy.Spider):
    name = "depthcharts"

    def start_requests(self):
        urls = ["https://www.cbssports.com/nba/injuries/","https://basketball.realgm.com/nba/depth-charts"]
        for url in urls:
            yield scrapy.Request(url=url, callback=self.parse)

    def parse(self, response):
        if response.url == "https://www.cbssports.com/nba/injuries/":
            filename = 'nba/injuries.html'
        else:
            filename = 'nba/depthcharts.html'
        with open(filename, 'wb') as f:
            f.write(response.body)
