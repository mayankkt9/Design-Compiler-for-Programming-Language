## 🎈 Rancho Programming Language

## ⚙  How to run your program
------------------
``` 
python3 main.py <inputfile>
Example 
python3 main.py sourcecode.rch
``` 

## 🕶  Project Video Link
------------------
* Youtube Link - ([Link 🚀](https://youtu.be/_Fm1NnSAL7U)) 


## ⚙  How to Install it on (MAC)
------------------

* Install SWI-Prolog Version 7.6.4 ([Click to Install 🚀](https://www.swi-prolog.org/download/stable/bin/SWI-Prolog-7.6.4.dmg)) 
	* Note this does not work for latest SWI-Prolog for version 8 or above because [this](https://github.com/yuce/pyswip/issues/17)
* In your /etc/profile add these lines 
```
export PATH=$PATH:/Applications/SWI-Prolog.app/Contents/swipl/bin/x86_64-darwin15.6.0
export DYLD_FALLBACK_LIBRARY_PATH=/Applications/SWI-Prolog.app/Contents/swipl/lib/x86_64-darwin15.6.0
``` 
* Make sure pip3 and python3 are installed on your mac and then run
```
pip3 install -r requirements.txt
```
* Run main.py present in src
``` 
python3 main.py inputfile
``` 
You can get input file from sample folder

## ⚙  How to Install it on (Windows)
------------------

* Install latest SWI-Prolog Version ([Click to Install 🚀](https://www.swi-prolog.org/download/stable/bin/swipl-8.0.3-1.x64.exe.envelope)) 

* Now add swipl in environment varibales. Go to path in enviroment variable and add following.
```
path_to_swipl\swipl\bin;
```
* Make sure pip and python are installed on your windows and then run
```
pip install -r requirements.txt
```
* Run main.py present in src
``` 
python main.py inputfile
``` 
You can get input file from sample folder


## ⚙  Tools Used
------------------
* SWI-Prolog Desktop Application ([Link 🚀](https://www.swi-prolog.org/download/stable/)) 
* Python Libraries Used - pyswip, tokenize, functools, io

