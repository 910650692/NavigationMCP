package com.fy.navi.service.logicpaket.l2;

import com.fy.navi.service.define.navi.L2NaviBean;

public interface L2InfoCallback {
    /**
     * l2 数据
     * @param l2NaviBean tbt数据
     * */
    default void onSdTbtDataChange(L2NaviBean l2NaviBean){

    }
}
