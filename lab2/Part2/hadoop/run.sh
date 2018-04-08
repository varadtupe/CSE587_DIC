#!/bin/bash

echo 
hdfs dfs -put -f lab2 lab2


echo 
hadoop jar lab2/hadoop-*streaming*.jar -file lab2/mapper.py -mapper lab2/mapper.py -file lab2/Top100reducer.py   -reducer lab2/Top100reducer.py -input lab2/input/protoNewsData/* -output lab2/output-protoNewsData
echo
hadoop jar lab2/hadoop-*streaming*.jar -file lab2/mapper.py -mapper lab2/mapper.py -file lab2/Top100reducer.py   -reducer lab2/Top100reducer.py -input lab2/input/protoTwitterData/* -output lab2/output-protoTwitterData

echo whole NY Times and Twitter data


echo
hdfs dfs -get lab2/output-protoNewsData /home/hadoop/lab2/output/
echo
hdfs dfs -get lab2/output-protoTwitterData /home/hadoop/lab2/output/



echo
hadoop jar lab2/hadoop-*streaming*.jar -file lab2/mapper.py -mapper lab2/mapper.py -file lab2/Top100reducer.py   -reducer lab2/Top100reducer.py -input lab2/input/NewsData/* -output lab2/output-NewsData
echo
hadoop jar lab2/hadoop-*streaming*.jar -file lab2/mapper.py -mapper lab2/mapper.py -file lab2/Top100reducer.py   -reducer lab2/Top100reducer.py -input lab2/input/TwitterData/* -output lab2/output-TwitterData

echo
hdfs dfs -get lab2/output-NewsData /home/hadoop/lab2/output/
echo
hdfs dfs -get lab2/output-TwitterData /home/hadoop/lab2/output/





echo
hadoop jar lab2/hadoop-*streaming*.jar -file lab2/pairMapper.py -mapper lab2/pairMapper.py -file lab2/Top20reducer.py   -reducer lab2/Top20reducer.py -input lab2/input/protoNewsData/* -output lab2/output-protoNewsData-wordPair
echo
hadoop jar lab2/hadoop-*streaming*.jar -file lab2/pairMapper.py -mapper lab2/pairMapper.py -file lab2/Top20reducer.py   -reducer lab2/Top20reducer.py -input lab2/input/protoTwitterData/* -output lab2/output-protoTwitterData-wordPair

echo
hdfs dfs -get lab2/output-protoNewsData-wordPair /home/hadoop/lab2/output/
echo
hdfs dfs -get lab2/output-protoTwitterData-wordPair /home/hadoop/lab2/output/
