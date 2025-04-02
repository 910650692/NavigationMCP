package com.fy.navi.service.define.layer.refix;

import com.fy.navi.service.define.search.ParkingInfo;

import java.util.List;

import lombok.Getter;
import lombok.Setter;

/**
 * BizCruiseCongestionInfo
 */
@Setter
@Getter
public class LayerItemSearchPark extends LayerItemBase {

    private List<ParkingInfo> parkingInfoList;

}
