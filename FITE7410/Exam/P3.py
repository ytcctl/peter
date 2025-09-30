# Write a Python program that reads one input line and analyze the letter frequency. 
# Input: 
#       - The program reads one input line tht may contain any symbol
#       - You can assume that the input has at least one valid letter (a-z, A-Z).
# Output:
#       - The program should output the top 5 frequently occuring letters in upper case, ordered alphabetically.
#       - Frequency analysis should be case insensitive
#       - If there is a draw, all letters with the same count should be included in the output.


import re

def top_5_occur_letters(a_string):
  a_string = re.sub(r"[^a-zA-Z]","",a_string)
  a_string = a_string.upper()
  a_string_list = list(a_string)
  a_string_set = set(a_string_list)
  
  string_dict ={}
  for i in a_string_set:
    k = 0
    for j in a_string_list:
      if i == j: 
        k += 1
    string_dict[i] = k
  
  values_count = string_dict.values()
  values_count_set = set(values_count)
  
  values_count_dict = {}
  for i in values_count_set:
    k = 0
    for j in values_count:
      if i == j:
        k += 1
    values_count_dict[i] = k

  Top5_value = []
  check = 0
  for k, v in sorted(values_count_dict.items(), reverse=True):
    Top5_value.append(k)
    check += v
    if check >= 5: break
  
  Top5 = []
  for k, v in string_dict.items():
    for j in Top5_value:
      if v == j:
        Top5.append(k)
  
  Top5 = sorted(Top5)
  Top5 = "".join(Top5)
    

  return Top5