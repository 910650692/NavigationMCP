package com.fy.navi.service.define.route;

public interface RouteRestirctionID {
    static final int    REATIRCTION_LIMITTIPSTYPEINVALID = -1;//无效值
    static final int 	REATIRCTION_LIMITTIPSTYPENOPLATE = 0;//外地车限行,建议设置车牌
    static final int 	REATIRCTION_LIMITTIPSTYPENOTOPEN = 1;//外地车限行,建议开启限行
    static final int 	REATIRCTION_LIMITTIPSTYPEAVOIDSUCCESS = 2;//已避开限行区域
    static final int 	REATIRCTION_LIMITTIPSTYPEREGIONSTART = 3;//起点在限行区域内
    static final int 	REATIRCTION_LIMITTIPSTYPEREGIONEND = 4;//终点在限行区域内
    static final int 	REATIRCTION_LIMITTIPSTYPEREGIONVIA = 5;//途经点在限行区域内
    static final int 	REATIRCTION_LIMITTIPSTYPEREGIONCROSS = 6;//途经限行区域
    static final int 	REATIRCTION_LIMITTIPSTYPEAVOIDFUTURESUCCESS = 7;//避开即将生效的限行，用户不能走完全程限行生效
    static final int 	REATIRCTION_LIMITTIPSTYPEEXPIREDIMMEDIATELY = 8;//已将失效的限行，用户到达已经失效
    static final int 	REATIRCTION_LIMITTIPSTYPEWAITLIMITOFF = 9;//途经区域XX时间限行即将结束,当前未回避开限行
    static final int 	REATIRCTION_LIMITTIPSTYPEWAITLIMITOFFSHORT = 10;//途经区域XX时间限行即将结束,当前已经回避开限行
    static final int 	REATIRCTION_LIMITTIPSTYPENETWORK = 10000;//网络不佳
}