package com.sgm.navi.scene.impl.map;

import static com.sgm.navi.service.MapDefaultFinalTag.MAP_TOUCH;

import com.android.utils.log.Logger;
import com.sgm.navi.burypoint.anno.HookMethod;
import com.sgm.navi.burypoint.constant.BuryConstant;
import com.sgm.navi.burypoint.controller.BuryPointController;
import com.sgm.navi.scene.BaseSceneModel;
import com.sgm.navi.scene.api.map.ISceneScale;
import com.sgm.navi.scene.impl.imersive.ImersiveStatus;
import com.sgm.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.sgm.navi.scene.ui.map.SceneScaleView;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.navistatus.NaviStatus;
import com.sgm.navi.service.logicpaket.map.MapPackage;
import com.sgm.navi.service.logicpaket.navistatus.NaviStatusPackage;

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
