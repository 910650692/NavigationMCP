package com.sgm.navi.vrbridge.bean;

import com.baidu.oneos.protocol.bean.PoiBean;

//多目的地导航中单个目的地信息
public class SingleDestInfo {

    private String mDestName; //名称
    private String mDestType; //类型
    private int mInNaviPlanType = PoiBean.NaviPlanType.DEFAULT; //途径点还是目的地

    public String getDestName() {
        return mDestName;
    }

    public void setDestName(final String destName) {
        mDestName = destName;
    }

    public String getDestType() {
        return mDestType;
    }

    public void setDestType(final String destType) {
        mDestType = destType;
    }

    public int getInNaviPlanType() {
        return mInNaviPlanType;
    }

    public void setInNaviPlanType(int mInNaviPlanType) {
        this.mInNaviPlanType = mInNaviPlanType;
    }
}
