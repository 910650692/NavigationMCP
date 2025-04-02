package com.fy.navi.service.define.layer.refix;

import com.fy.navi.service.define.search.ChargeInfo;

import java.util.List;

import lombok.Getter;
import lombok.Setter;

/**
 * BizCruiseCongestionInfo
 */
@Setter
@Getter
public class LayerItemSearchChargeStation extends LayerItemBase {

    private List<ChargeInfo> chargeInfoList;

}
