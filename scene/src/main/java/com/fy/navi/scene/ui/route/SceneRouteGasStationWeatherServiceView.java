package com.fy.navi.scene.ui.route;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.android.utils.ConvertUtils;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.R;
import com.fy.navi.scene.api.route.ISceneRouteGasStationWeatherServiceSelectCallBack;
import com.fy.navi.scene.api.route.ISceneRouteSelectCallBack;
import com.fy.navi.scene.databinding.SceneRouteResultListViewBinding;
import com.fy.navi.scene.databinding.SceneRouteStationWeatherServiceBinding;
import com.fy.navi.scene.impl.route.SceneRouteGasStationWeatherServiceImpl;
import com.fy.navi.scene.impl.route.SceneRouteResultListImpl;
import com.fy.navi.scene.ui.adapter.RouteResultAdapter;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.define.route.RouteLineInfo;

import java.util.Hashtable;
import java.util.List;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/2
 */
public class SceneRouteGasStationWeatherServiceView extends BaseSceneView<SceneRouteStationWeatherServiceBinding, SceneRouteGasStationWeatherServiceImpl> {

    private Hashtable<String, ISceneRouteGasStationWeatherServiceSelectCallBack> sceneRouteGasStationWeatherServiceSelectCallBack;

    public SceneRouteGasStationWeatherServiceView(@NonNull Context context) {
        super(context);
    }

    public SceneRouteGasStationWeatherServiceView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneRouteGasStationWeatherServiceView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected SceneRouteStationWeatherServiceBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
        return SceneRouteStationWeatherServiceBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneRouteGasStationWeatherServiceImpl initSceneImpl() {
        sceneRouteGasStationWeatherServiceSelectCallBack = new Hashtable<>();
        return new SceneRouteGasStationWeatherServiceImpl(this);
    }

    public void registerRouteSelectObserver(String key, ISceneRouteGasStationWeatherServiceSelectCallBack callBack) {
        sceneRouteGasStationWeatherServiceSelectCallBack.put(key, callBack);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setScene(mScreenViewModel);
    }

    @Override
    protected void initObserver() {
    }
    public void updateUi() {
        if (ConvertUtils.isEmpty(mViewBinding) || ConvertUtils.isEmpty(mScreenViewModel)) return;
        mViewBinding.routeRightTabListTvGasStation.setSelected(mScreenViewModel.isGasStationSelect);
        mViewBinding.routeRightTabListIvGasStation.setSelected(mScreenViewModel.isGasStationSelect);
        mViewBinding.routeRightTabListTvWeather.setSelected(mScreenViewModel.isWeatherSelect);
        mViewBinding.routeRightTabListIvWeather.setSelected(mScreenViewModel.isWeatherSelect);
        mViewBinding.routeRightTabListTvService.setSelected(mScreenViewModel.isServiceSelect);
        mViewBinding.routeRightTabListIvService.setSelected(mScreenViewModel.isServiceSelect);
    }

    public void clearSceneTabUI() {
        mScreenViewModel.isGasStationSelect = false;
        mScreenViewModel.isWeatherSelect = false;
        mScreenViewModel.isServiceSelect = false;
        updateUi();
    }

    public void clickTab(int tabIndex, boolean isChceked) {
        for (ISceneRouteGasStationWeatherServiceSelectCallBack callBack : sceneRouteGasStationWeatherServiceSelectCallBack.values()) {
            if (ConvertUtils.isEmpty(callBack)) continue;
            callBack.onTabListClick(tabIndex, isChceked);
        }
    }
}
