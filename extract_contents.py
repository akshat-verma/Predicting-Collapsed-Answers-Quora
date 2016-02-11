'''
Created on Nov 25, 2015

@author: akshat
'''

from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.keys import Keys
from bs4 import BeautifulSoup
import time
import json
import urllib.request
from selenium.common.exceptions import NoSuchElementException
import urllib.parse
import traceback


with open('questions.json') as data_file:    
    topic_dict = json.load(data_file)   
    

chrome_options = Options()
browser = webdriver.Chrome("/Users/akshat/Documents/workspace/Stat6021/chromedriver")
browser.get("https://www.quora.com/")
form = browser.find_element_by_class_name('regular_login')
username = form.find_element_by_name('email')
username.send_keys('quorabot@gmail.com')
password = form.find_element_by_name('password')
password.send_keys('stat6018')
password.send_keys(Keys.RETURN)

time.sleep(0.5)
def trim_init_char(text):
    if text.startswith("Originally Answered:"):
        return text[text.find("?")+1:]
    else: 
        return text

def trim_extra_char(text):
    if text.rfind("Updated") > -1:
        return text.rfind("Updated")
    else:
        return text.rfind("Written")
    
def remove_non_ascii(text):
    return text.replace(u'\xa0', u' ')

def urlformat(u):
    s, a, p, q, f = urllib.parse.urlsplit(u)
    p = urllib.parse.quote(p)
    return urllib.parse.urlunsplit((s, a, p, q, f))

f = open("quora.csv","a",encoding='utf-8')

for topic in topic_dict:
    print(topic)
    for link in topic_dict[topic]:
        try:
            print (link)
            browser.get(link) 
            src_updated = browser.page_source
            src = ""
            page_down_counter = 0
            while src != src_updated:
                time.sleep(2)
                src = src_updated
                browser.execute_script("window.scrollTo(0, document.body.scrollHeight);")
                src_updated = browser.page_source
                page_down_counter = page_down_counter+1
            
            try: 
                link = browser.find_element_by_partial_link_text('Answers Collapsed')
                
            except NoSuchElementException:
                continue
            browser.execute_script("window.scrollBy(0, -150)")
            time.sleep(2)

            link.click()
            time.sleep(20)
                
            html_source = browser.page_source
            soup = BeautifulSoup(html_source,'html.parser')
            q = soup.find("div", { "class" : "QuestionTextInlineEditor InlineEditor inline_editor_content" })
            q_text = q.find("span", { "class" : "inline_editor_value" }).get_text(strip=True)
            answers = [td.find("span", { "class" : "inline_editor_value" }) for td in soup.findAll("div", { "class" : "Answer AnswerBase" })]
            answers_text = [answer.get_text(strip=True) for answer in answers]
            answers_text = [remove_non_ascii(trim_init_char(text[:trim_extra_char(text)])) for text in answers_text ]
           
            user_profile = []
            user_questions = []
            user_answers = []
            user_posts = []
            user_followers = []
            
            collapsed_user_profile = []
            collapsed_user_questions = []
            collapsed_user_answers = []
            collapsed_user_posts = []
            collapsed_user_followers = []
            
            for td in soup.findAll("div", { "class" : "Answer AnswerBase" }):
                if td.find("a", { "class" : "user" }) is not None:
                    user_profile.append("https://www.quora.com/"+td.find("a", { "class" : "user" })['href'])
                else:
                    user_profile.append("Anon")
            for user in user_profile:
                if user =="Anon":
                    user_questions.append("Anon")
                    user_answers.append("Anon")
                    user_posts.append("Anon")
                    user_followers.append("Anon")
                else:
                    user_page = urllib.request.urlopen(urlformat(user))
                    user_info = BeautifulSoup(user_page.read(),'html.parser')
                    user_counts = user_info.findAll("span", { "class" : "list_count" })
                    user_questions.append(user_counts[0].get_text(strip=True))
                    user_answers.append(user_counts[1].get_text(strip=True))
                    user_posts.append(user_counts[2].get_text(strip=True))
                    user_followers.append(user_counts[3].get_text(strip=True))
            
            
            collapsed_answers = [td.find("span", { "class" : "inline_editor_value" }) for td in soup.findAll("div", { "class" : "Answer AnswerBase undesirable_answer" })]
            collapsed_answers_text = [answer.get_text(strip=True) for answer in collapsed_answers]
            collapsed_answers_text = [trim_init_char(remove_non_ascii(text[:trim_extra_char(text)])) for text in collapsed_answers_text ]
            for td in soup.findAll("div", { "class" : "Answer AnswerBase undesirable_answer" }):
                if td.find("a", { "class" : "user" }) is not None:
                    collapsed_user_profile.append("https://www.quora.com/"+td.find("a", { "class" : "user" })['href'])
                else:
                    collapsed_user_profile.append("Anon")
            for user in collapsed_user_profile:
                if user =="Anon":
                    collapsed_user_questions.append("Anon")
                    collapsed_user_answers.append("Anon")
                    collapsed_user_posts.append("Anon")
                    collapsed_user_followers.append("Anon")
                else:
                    user_page = urllib.request.urlopen(urlformat(user))
                    user_info = BeautifulSoup(user_page.read(),'html.parser')
                    user_counts = user_info.findAll("span", { "class" : "list_count" })
                    collapsed_user_questions.append(user_counts[0].get_text(strip=True))
                    collapsed_user_answers.append(user_counts[1].get_text(strip=True))
                    collapsed_user_posts.append(user_counts[2].get_text(strip=True))
                    collapsed_user_followers.append(user_counts[3].get_text(strip=True))
           
            upvotes = [td.find("span", { "class" : "count" }).get_text(strip=True) for td in soup.find("div", { "class" : "content_page_feed_offset" }).findAll("a", { "class" : "Answer Upvote Button TwoStateButton primary_action answer_upvote main_button" })]
            views = [str(td.find("div", { "class" : "CredibilityFact" }).text).split(" ")[0] for td in soup.find("div", { "class" : "content_page_feed_offset" }).findAll("div", { "class" : "credibility_facts" })]
            print(len(answers_text))
            for i in range(0,len(answers_text)):
                f.write("A|"+topic+ "|\"" + q_text +"\""+"|\""+answers_text[i]+"\"|"+upvotes[i]+"|"+views[i]+"|"+user_questions[i]+"|"+user_answers[i]+"|"+user_posts[i]+"|"+user_followers[i]+"\n")
                f.flush()
            print(len(collapsed_answers_text))   
            for i in range(0|len(collapsed_answers_text)):
                f.write("C|"+topic+"|\"" + q_text+"\""+"|\""+ collapsed_answers_text[i]+"\"|"+upvotes[len(answers_text)+i]+"|NA|"+collapsed_user_questions[i]+"|"+collapsed_user_answers[i]+"|"+collapsed_user_posts[i]+"|"+collapsed_user_followers[i]+"\n")
                f.flush()
            
        except Exception:
            print (traceback.format_exc())
            continue
            


    

           
                 
