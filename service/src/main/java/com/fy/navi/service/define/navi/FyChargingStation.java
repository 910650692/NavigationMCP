package com.fy.navi.service.define.navi;

import com.fy.navi.service.define.route.RouteChargeStationDetailInfo;

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class FyChargingStation implements Serializable {
    // 电动汽车的能量消耗总量按行程点顺序排列 如:充电站1、充电站2、单位百分之一wh，有符号
    private long chargeEnrgySum;
    private RouteChargeStationDetailInfo chargeInfo;
}
