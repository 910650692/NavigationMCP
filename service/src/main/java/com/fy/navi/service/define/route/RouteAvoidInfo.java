package com.fy.navi.service.define.route;

import java.util.ArrayList;

public class RouteAvoidInfo {

    private boolean checkedLeastOne = false;
    private ArrayList<Long> avoidList = new ArrayList<>();

    public ArrayList<Long> getAvoidList() {
        return avoidList;
    }

    public void setAvoidList(ArrayList<Long> avoidList) {
        this.avoidList = avoidList;
    }

    public boolean getCheckedLeastOne() {
        return checkedLeastOne;
    }

    public void setCheckedLeastOne(boolean selectAll) {
        checkedLeastOne = selectAll;
    }
}
