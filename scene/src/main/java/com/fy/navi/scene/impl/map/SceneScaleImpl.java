package com.fy.navi.scene.impl.map;

import static com.fy.navi.service.MapDefaultFinalTag.MAP_TOUCH;

import com.android.utils.log.Logger;
import com.fy.navi.burypoint.anno.HookMethod;
import com.fy.navi.burypoint.constant.BuryConstant;
import com.fy.navi.burypoint.controller.BuryPointController;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.map.ISceneScale;
import com.fy.navi.scene.impl.imersive.ImersiveStatus;
import com.fy.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.fy.navi.scene.ui.map.SceneScaleView;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.navistatus.NaviStatusPackage;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/2
 */
public class SceneScaleImpl extends BaseSceneModel<SceneScaleView> implements ISceneScale {
    private MapPackage mapPackage;

    public SceneScaleImpl(SceneScaleView mScreenView) {
        super(mScreenView);
        mapPackage = MapPackage.getInstance();
    }

    private void naviScaleClick(){
        if (NaviStatus.NaviStatusType.NAVING.equals(NaviStatusPackage.getInstance().getCurrentNaviStatus())) {
            Logger.d("SceneScaleImpl" ,MAP_TOUCH);
            ImmersiveStatusScene.getInstance().setImmersiveStatus(MapType.MAIN_SCREEN_MAIN_MAP, ImersiveStatus.TOUCH);
        }
    }

    @Override
    public void reduceLevel() {
        Logger.i("lvww", "缩小比例尺");
        mapPackage.reduceLevel(MapType.MAIN_SCREEN_MAIN_MAP);
        naviScaleClick();
        if(NaviStatusPackage.getInstance().isGuidanceActive()) sendBuryPointForZoomByClick(BuryConstant.ZoomAction.ZOOM_OUT);
    }

    @Override
    public void amplifyLevel() {
        Logger.i("lvww", "放大比例尺");
        mapPackage.amplifyLevel(MapType.MAIN_SCREEN_MAIN_MAP);
        naviScaleClick();
        if(NaviStatusPackage.getInstance().isGuidanceActive()) sendBuryPointForZoomByClick(BuryConstant.ZoomAction.ZOOM_IN);
    }

    @HookMethod
    private void sendBuryPointForZoomByClick(BuryConstant.ZoomAction zoomAction) {
        String eventName = switch (zoomAction){
            case ZOOM_IN -> BuryConstant.EventName.AMAP_NAVI_MAP_MANUAL_EMPLIFY_CLICK;
            case ZOOM_OUT -> BuryConstant.EventName.AMAP_NAVI_MAP_MANUAL_REDUCE_CLICK;
        };
        BuryPointController.getInstance().setEventName(eventName);
    }
}
