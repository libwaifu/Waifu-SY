# 词表法

---

所谓词表法, 说白了就是搞个数据库, 然后从里面抽就是了.

原始数据库 : https://github.com/chinese-poetry/chinese-poetry-zhCN

#### 使用

如果 raw 文件夹为空, 请运行 `!get.sh` 脚本同步数据.

然后直接加载 `Tableaux.m` 程序包即可.

#### 函数: 

> UtahimeTableaux[long,str]
>> long 表示所有诗词的长度\
>> str 表示需要的藏头诗词


##### In: 
```mathematica
UtahimeTableaux[7,"苟利国家"]//TableForm
```

##### Out:
```mathematica
苟溯风而遐契兮
利尽耕桑岂有穷
国风雅颂兴赋比
家藏履舄星辰旧
```