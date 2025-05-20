package com.fy.navi.hmi.map;

import android.app.Application;

import androidx.annotation.NonNull;
import com.fy.navi.service.BuildConfig;

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

    public void initVisibleAreaPoint(){
        mModel.loadVisibleAreaJson(jsonPath);
    }
}
