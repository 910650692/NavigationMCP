package com.sgm.navi.hmi.map;

import android.app.Application;

import androidx.annotation.NonNull;

import com.android.utils.log.Logger;
import com.sgm.navi.hmi.BuildConfig;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/26
 */
public class MapViewModel extends BaseMapViewModel {

    private String jsonPath = BuildConfig.MAP_SDK + "/buick_maparea.json";

    public MapViewModel(@NonNull Application application) {
        super(application);
    }

    @Override
    public void onCreate() {
        super.onCreate();
    }


    public void initVisibleAreaPoint(){
        mModel.loadVisibleAreaJson(jsonPath);
    }

    public boolean showNdGoHomeView(){
        return false;
    }

    public void addSceneGoHomeCallBack(int type){
        mModel.addSceneGoHomeCallBack(type);
    }

}
