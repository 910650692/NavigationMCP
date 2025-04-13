package com.fy.navi.service.logicpaket.l2;

public interface L2InfoCallback {
    /**
     * l2 数据
     * @param json tbt数据
     * */
    default void onSdTbtDataChange(String json){

    }
}
