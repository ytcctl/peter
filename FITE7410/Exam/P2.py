# Write a Python program for validating credit card numbers using the Luhn algorithm
# The steps to validate a credit number using the Luhn algorithm are as follows:
# 1. Starting from the rightmost digit (that is the check digit), double the value of every second digit.
# 2.  If the doubled value is a two-digit number, sum the digits of that number together to form a single digit.
# 3. Add all the 16 digits together
# 4. if the final sum is divisible by 10, then the credit card number is valid; if it is not divisible by 10, 
#    then the number is invalid or fake.

# Input: 
#       - The program reads one credit card number from user.
#       - You can assume that the input is always a 16--digit number. and each digit is in the range [0,9].
#         There is no need to consider invvalid inputs. 
# Output:
#       - The program should output a message reporting the validility of the credit card number. 

def check_credit_card(credit_card_number):
  str_credit_card_number = str(credit_card_number)
  if len(str_credit_card_number) != 16:
    return str_credit_card_number + " is invalid."
  list_card_digit = [int(d) for d in str_credit_card_number]
  list_card_digit.reverse()
  a = 0
  b = 0

  for i in range(0,16, 2):
    a += list_card_digit[i]


  for j in range(1,16, 2):
      num = (list_card_digit[j] * 2)
      if num <10:
        a += num
      else:
        num_list = [int(k) for k in str(num)]
        a += (num_list[0] + num_list[1])

  if a % 10 == 0:
    return str_credit_card_number + " is valid."
  else:
    return str_credit_card_number + " is invalid."
