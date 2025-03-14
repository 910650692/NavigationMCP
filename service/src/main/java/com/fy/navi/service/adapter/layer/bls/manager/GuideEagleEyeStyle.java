package com.fy.navi.service.adapter.layer.bls.manager;

import com.android.utils.thread.RunTask;
import com.android.utils.thread.ThreadManager;
import com.autonavi.gbl.layer.BizControlService;
import com.autonavi.gbl.layer.model.EagleEyeStyle;
import com.autonavi.gbl.map.MapView;
import com.autonavi.gbl.map.model.MapViewPortParam;

public class GuideEagleEyeStyle extends BaseLayerStyle {
    protected GuideEagleEyeStyle(BizControlService bizService, MapView mapView) {
        super(bizService, mapView);
    }

    /**
     * 初始化鹰眼图，在独立线程执行
     *
     * @param style EagleEyeStyle
     */
    public void initEagleEyeAsync(EagleEyeStyle style) {
        ThreadManager.getInstance().supplyAsync(new RunTask<>() {
            @Override
            public void run() {
//                if (null == mEagleEyePrepareLayerStyle) {
//                    Logger.e(TAG, "initEagleEye  mEagleEyePrepareLayerStyle is null!", new NullPointerException());
//                    return;
//                }
//                if (!mGuideEagleEyeControl.isInitialized()) {
//                    mGuideEagleEyeControl.init(style, mEagleEyePrepareLayerStyle);
//                }
//                MapViewPortParam mapViewPortParam = new MapViewPortParam();
//                mapViewPortParam.x = style.mapViewParam.x;
//                mapViewPortParam.y = style.mapViewParam.y;
//                mapViewPortParam.width = style.mapViewParam.width;
//                mapViewPortParam.height = style.mapViewParam.height;
//                mapViewPortParam.screenWidth = style.mapViewParam.screenWidth;
//                mapViewPortParam.screenHeight = style.mapViewParam.screenHeight;
//                updateEagleEyeParam(style.eagleEyeParam);
//                mGuideEagleEyeControl.updateStyle(style.isNightMode);
//                updateMapViewPort(mapViewPortParam);
//                showEaglePath();
            }
        });
    }

    /**
     * 释放鹰眼图，在独立线程执行
     */
    public void unInitEagleEyeAsync() {
        ThreadManager.getInstance().supplyAsync(new RunTask<>() {
            @Override
            public void run() {
                if (mGuideEagleEyeControl != null) {
                    mGuideEagleEyeControl.unInit();
                }
            }
        });
    }
}
