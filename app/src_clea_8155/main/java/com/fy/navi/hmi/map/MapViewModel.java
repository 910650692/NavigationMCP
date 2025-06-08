package com.fy.navi.hmi.map;

import android.app.Application;

import androidx.annotation.NonNull;

import com.fy.navi.hmi.utils.ScreenTypeUtils;
import com.fy.navi.service.BuildConfig;

import com.android.utils.log.Logger;
import com.fy.navi.service.define.screen.ScreenType;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/26
 */
public class MapViewModel extends BaseMapViewModel {

    //   ND全屏
    private String jsonPath = BuildConfig.MAP_SDK + "/nd_maparea.json";
    //   2/3屏幕
    private String jsonPath_2_3 = BuildConfig.MAP_SDK + "/nd_2_3_maparea.json";
    //   1/3屏幕
    private String jsonPath_1_3 = BuildConfig.MAP_SDK + "/nd_1_3_maparea.json";

    private static final String TAG = "NDLB---MapViewModel";
    public MapViewModel(@NonNull Application application) {
        super(application);
    }

    public void initVisibleAreaPoint(){
        mModel.loadVisibleAreaJson(jsonPath);
    }

    public boolean showNdGoHomeView(){
        return true;
    }

    public void addSceneGoHomeCallBack(int type){
        mModel.addSceneGoHomeCallBack(type);
    }

//    @Override
//    void setScreenType(int right) {
//        super.setScreenType(right);
//        // 1/2: 1084, 1/3 739, 2/3 1430, 全屏：2179
//        if (right <= 800) {
//            Logger.i(TAG, "切换1/3屏");
//            ScreenTypeUtils.setScreenType(ScreenType.SCREEN_1_3);
//            mModel.ndChangeScreen(jsonPath_1_3,true);
//            //todo 启动1/3屏activity
//        } else if (right > 1100 && right < 1600) {
//            Logger.i(TAG, "切换2/3屏");
//            ScreenTypeUtils.setScreenType(ScreenType.SCREEN_2_3);
//            mModel.ndChangeScreen(jsonPath_2_3,true);
//        } else if (right <= 2179) {
//            Logger.i(TAG, "全屏");
//            mModel.ndChangeScreen(jsonPath,false);
//        }
//    }
}
