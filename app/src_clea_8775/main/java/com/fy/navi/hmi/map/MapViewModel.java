package com.fy.navi.hmi.map;

import android.app.Application;

import androidx.annotation.NonNull;

import com.fy.navi.hmi.splitscreen.SplitScreenManager;
import com.fy.navi.hmi.utils.ScreenTypeUtils;
import com.fy.navi.service.define.screen.ScreenType;
import com.fy.navi.ui.action.Action;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/26
 */
public class MapViewModel extends BaseMapViewModel {
    private static final String TAG = "NDLB---MapViewModel--8775";
    public MapViewModel(@NonNull Application application) {
        super(application);
    }

    public void initVisibleAreaPoint() {
        mModel.loadVisibleAreaJson(SplitScreenManager.getInstance().getScreenJsonPath());
    }

    public boolean showNdGoHomeView() {
        return true;
    }

    public void addSceneGoHomeCallBack(int type) {
        mModel.addSceneGoHomeCallBack(type);
    }

    public Action switchSr = () -> {
        if (ScreenTypeUtils.getScreenType() == ScreenType.SCREEN_2_3) {
            SplitScreenManager.getInstance().switchSRToFullScreen();
        } else {
            SplitScreenManager.getInstance().switchSRToOneThirdScreen();
        }
    };

    @Override
    public boolean isSupportSplitScreen() {
        return super.isSupportSplitScreen();
    }
}
