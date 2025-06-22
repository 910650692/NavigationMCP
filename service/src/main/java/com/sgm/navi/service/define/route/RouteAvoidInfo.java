package com.sgm.navi.service.define.route;

import java.util.ArrayList;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RouteAvoidInfo {
    private boolean mCheckedLeastOne = false;
    private ArrayList<Long> mAvoidList = new ArrayList<>();
}
