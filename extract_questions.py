'''
Created on Nov 25, 2015

@author: akshat
'''


from bs4 import BeautifulSoup
import json
import urllib.request


topics_list_ques = urllib.request.urlopen("https://www.quora.com/What-are-the-most-followed-topics-on-Quora-in-2015")
topics_list_page = BeautifulSoup(topics_list_ques.read(),'html.parser')
 
topics_list = ["https://www.quora.com"+link.find("a")['href'] for link in topics_list_page.find("div", { "class" : "Answer AnswerBase" }).find("ol").findAll("span", { "class" : "qlink_container" })]
 
topic_dict = {}

for link in topics_list:
    topic_page = urllib.request.urlopen(link)
    topic = BeautifulSoup(topic_page.read(),'html.parser')
    questions = ["https://www.quora.com"+question['href'] for question in topic.findAll("a", { "class" : "question_link" })]
    topic_dict[link[link.rfind("/")+1:]] = questions
     
with open('questions.json', 'w') as outfile:
    json.dump(topic_dict, outfile)

