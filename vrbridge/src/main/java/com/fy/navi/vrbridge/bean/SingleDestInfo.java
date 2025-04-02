package com.fy.navi.vrbridge.bean;

//多目的地导航中单个目的地信息
public class SingleDestInfo {

    private String mDestName; //名称
    private String mDestType; //类型

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

}
