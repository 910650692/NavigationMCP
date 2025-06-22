package com.sgm.navi.service.define.navi;

import com.sgm.navi.service.define.bean.GeoPoint;

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class FyViaMergeInfo implements Serializable {
    private String poiName;
    private GeoPoint geoPoint;
}
