#!/usr/bin/python

import sys
# something

# get total number of arguments
total = len(sys.argv)

# get the arguments list
cmdargs = str(sys.argv)

# print the arguments
print("The total numbers of args passed to the script: %d " % total)
print("Args list: %s " % cmdargs)

# create script inputs from arguments
age_arg = sys.argv[1]
sex_arg = int(float((sys.argv[2])))

sex_lookup =['male','female']

print("The age chosen is %s " % age_arg)
print("The sex chosen is %d " % sex_arg)
print("The sex chosen is %s " % sex_lookup[(sex_arg-1)])



