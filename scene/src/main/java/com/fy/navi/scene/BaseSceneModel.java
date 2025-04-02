package com.fy.navi.scene;

import android.os.Bundle;

import androidx.lifecycle.Lifecycle;
import androidx.lifecycle.LifecycleObserver;
import androidx.lifecycle.OnLifecycleEvent;

import com.android.utils.log.Logger;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.ui.base.BaseFragment;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/2
 */
public class BaseSceneModel<V extends BaseSceneView> implements LifecycleObserver {
    protected V mScreenView;
    protected MapType mMapTypeId;

    protected BaseSceneModel(V mScreenView) {
        this.mScreenView = mScreenView;
    }

    @OnLifecycleEvent(Lifecycle.Event.ON_CREATE)
    protected void onCreate() {
        Logger.d(getClass().getSimpleName(), "Scene onCreate");
    }

    @OnLifecycleEvent(Lifecycle.Event.ON_DESTROY)
    protected void onDestroy() {
        Logger.d(getClass().getSimpleName(), "Scene onDestroy");
    }

    protected void setScreenId(MapType mapTypeId){
        this.mMapTypeId = mapTypeId;
    }

    protected void addFragment(BaseFragment fragment, Bundle bundle){
        mScreenView.addFragment(fragment, bundle);
    }
}
