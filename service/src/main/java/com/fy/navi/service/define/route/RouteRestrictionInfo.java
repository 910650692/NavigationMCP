package com.fy.navi.service.define.route;

import java.util.ArrayList;

public class RouteRestrictionInfo {
    public String title;
    public String desc;
    public String tips;
    public int cityCode;
    public short type;
    public short titleType;
    public ArrayList<Long> ruleIDs;
    public ArrayList<Short> tailNums;

    public RouteRestrictionInfo() {
        this.title = "";
        this.desc = "";
        this.tips = "";
        this.cityCode = 0;
        this.type = 0;
        this.titleType = 0;
        this.ruleIDs = new ArrayList();
        this.tailNums = new ArrayList();
    }

    public RouteRestrictionInfo(String titleLiteObj, String descLiteObj, String tipsLiteObj, int cityCodeLiteObj, short typeLiteObj, short titleTypeLiteObj, ArrayList<Long> ruleIDsLiteObj, ArrayList<Short> tailNumsLiteObj) {
        this.title = titleLiteObj;
        this.desc = descLiteObj;
        this.tips = tipsLiteObj;
        this.cityCode = cityCodeLiteObj;
        this.type = typeLiteObj;
        this.titleType = titleTypeLiteObj;
        this.ruleIDs = ruleIDsLiteObj;
        this.tailNums = tailNumsLiteObj;
    }
}
