package com.sgm.navi.hmi.map;

import android.app.Application;

import androidx.annotation.NonNull;

import com.android.utils.log.Logger;
import com.sgm.navi.hmi.splitscreen.SplitScreenManager;
import com.sgm.navi.service.BuildConfig;
import com.android.utils.screen.ScreenTypeUtils;
import com.sgm.navi.ui.action.Action;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/26
 */
public class MapViewModel extends BaseMapViewModel {
    private final String FULL_SCREEN_JSON_PATH = BuildConfig.MAP_SDK + "/nd_maparea.json";
    private final String TWO_THIRD_JSON_PATH = BuildConfig.MAP_SDK + "/nd_2_3_maparea.json";
    private final String ONE_THIRD_JSON_PATH = BuildConfig.MAP_SDK + "/nd_1_3_maparea.json";
    public MapViewModel(@NonNull Application application) {
        super(application);
    }

    public void initVisibleAreaPoint() {
        Logger.d("screen_change_used", "设置屏幕投影位置");
        String json;
        if (ScreenTypeUtils.getInstance().isFullScreen()) {
            json = FULL_SCREEN_JSON_PATH;
        } else if (ScreenTypeUtils.getInstance().isTwoThirdScreen()) {
            json = TWO_THIRD_JSON_PATH;
        } else {
            json = ONE_THIRD_JSON_PATH;
        }
        mModel.loadVisibleAreaJson(json);
    }

    public boolean showNdGoHomeView() {
        return true;
    }

    public void addSceneGoHomeCallBack(int type) {
        mModel.addSceneGoHomeCallBack(type);
    }

    public Action switchSr = () -> {
        if (ScreenTypeUtils.getInstance().isFullScreen()) {
            Logger.d("screen_change_used", "切换到2/3屏幕");
            SplitScreenManager.getInstance().switchSRToOneThirdScreen(mView.getTaskId());
        } else {
            Logger.d("screen_change_used", "切换到全屏幕");
            SplitScreenManager.getInstance().switchSRToFullScreen();
        }
    };

    @Override
    public boolean isSupportSplitScreen() {
        return true;
    }
}
