package com.sgm.navi.service.logicpaket.l2;

import com.sgm.navi.service.define.navi.L2NaviBean;

public interface L2InfoCallback {
    /**
     * l2 数据
     * @param l2NaviBean tbt数据
     * */
    default void onSdTbtDataChange(L2NaviBean l2NaviBean){

    }
}
