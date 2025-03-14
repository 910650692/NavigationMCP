package com.fy.navi.service.adapter.layer.bls.manager;

import com.android.utils.ConvertUtils;
import com.autonavi.gbl.layer.BizControlService;
import com.autonavi.gbl.map.MapView;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/9
 */
public class NaviLayerStyle extends BaseLayerStyle {


    public NaviLayerStyle(BizControlService bizService, MapView mapView) {
        super(bizService, mapView);
    }

    @Override
    protected void unInit() {
        if(!ConvertUtils.isEmpty(mGuideEagleEyeControl)) mGuideEagleEyeControl = null;
    }
}
