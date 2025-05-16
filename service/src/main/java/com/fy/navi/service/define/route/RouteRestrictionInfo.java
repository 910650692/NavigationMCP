package com.fy.navi.service.define.route;

import java.io.Serializable;
import java.util.ArrayList;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RouteRestrictionInfo implements Serializable {
    private String mTitle;
    private String mDesc;
    private String mTips;
    private int mCityCode;
    private short mType;
    private short mTitleType;
    private ArrayList<Long> mRuleIDs;
    private ArrayList<Short> mTailNums;
}
