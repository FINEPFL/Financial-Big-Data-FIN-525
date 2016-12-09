from __future__ import division
from matplotlib import pyplot as plt

from sklearn import svm

import numpy as np
import sklearn
import operator
import csv

def determiner(nums, mean_list):
     counter = sum(1 for x in map(operator.sub, nums, mean_list) if x > 0)
     if counter >= 5:
         return 1
     return 0


def get_means():
    out_mean_list = []
    with open("commes_mean.csv", "rb") as f:
        row = csv.reader(f, delimiter=",")
        for item in row:
            for num in item:
                out_mean_list.append(float(num))
    return out_mean_list

def add_tag(filename, mean_list):
    out_tensor = []
    tags = []
    with open(filename, "rb") as f:
        rows = csv.reader(f, delimiter=",")
        for item in rows:
            out_row_tuple = []
            for num in item:
                out_row_tuple.append(float(num))
            tag = determiner(out_row_tuple, mean_list)
            tags.append(tag)
            out_tensor.append(out_row_tuple)
        return out_tensor, tags

mean_list = get_means()
full_tensor, tags = (add_tag("raw_data.csv", mean_list))

test_mistake_rate_list = []
train_mistake_rate_list = []
test_size_list = [1500, 1300, 1000, 800, 600]

for test_size in test_size_list:

    X =  full_tensor[:-test_size]
    y =  tags[:-test_size]

    test_set = full_tensor[-test_size:]
    test_tags = y[-test_size:]

    clf = svm.SVC(C=100.0, kernel="rbf",gamma=0.001)
    clf.fit(X, y)

    # train_mistake_rate_list.append(sum(abs(np.asarray(map(operator.sub,
    # y, clf.predict(X)))))/(3701-test_size))
    # # print y, clf.predict(X)

    test_mistake_rate_list.append(sum(abs(np.asarray(map(operator.sub,
    test_tags, clf.predict(test_set)))))/test_size)

print test_mistake_rate_list






#
