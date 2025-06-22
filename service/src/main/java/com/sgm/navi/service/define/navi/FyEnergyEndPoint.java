package com.sgm.navi.service.define.navi;

import com.sgm.navi.service.define.bean.GeoPoint;

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class FyEnergyEndPoint implements Serializable {
    // 	电量耗尽点位置
    private GeoPoint show;
    private short segmentIdx;
    private short linkIndex;
}
