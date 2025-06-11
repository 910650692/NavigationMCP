package com.fy.navi.hmi.map;

import android.app.Application;

import androidx.annotation.NonNull;

import com.fy.navi.hmi.splitscreen.SplitScreenManager;
import com.fy.navi.service.BuildConfig;
import com.fy.navi.ui.action.Action;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/26
 */
public class MapViewModel extends BaseMapViewModel {
    private static final String TAG = "NDLB---MapViewModel";
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
        SplitScreenManager.getInstance().switchSR();
    };
}
