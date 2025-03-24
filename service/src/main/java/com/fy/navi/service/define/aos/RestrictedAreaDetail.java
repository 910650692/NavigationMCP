package com.fy.navi.service.define.aos;

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RestrictedAreaDetail implements Serializable {
    private long mRequestId;
    private int mCityCode;           //1100
    private String mCityName;        //北京市
    private String mTitle;           //北京市限行政策
    private String mPolicyname;       //二环及以内外地机动车限行
    private int mRuleid;              //1766888
    private int mRing;                //0
    private int mEffect;              //1
    private int mLocal;               //2
    private int mVehicle;             //1
    private String mTime;             //2021年11月1日起，工作日和周末全天限行（节假日限制）
    private String mSummary;          //二环路及以内道路（不含外侧辅路）
    private String mDesc;             //外地机动车禁行
    private String mOtherdesc;        //“”
}
