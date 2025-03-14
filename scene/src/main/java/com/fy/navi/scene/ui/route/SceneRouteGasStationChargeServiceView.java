package com.fy.navi.scene.ui.route;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.ConvertUtils;
import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.api.route.ISceneRouteGasStationChargeSelectCallBack;
import com.fy.navi.scene.api.route.ISceneRouteGasStationWeatherServiceSelectCallBack;
import com.fy.navi.scene.databinding.SceneRouteStationChargeServiceBinding;
import com.fy.navi.scene.databinding.SceneRouteStationWeatherServiceBinding;
import com.fy.navi.scene.impl.route.SceneRouteGasStationChargeServiceImpl;
import com.fy.navi.scene.impl.route.SceneRouteGasStationWeatherServiceImpl;

import java.util.Hashtable;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/2
 */
public class SceneRouteGasStationChargeServiceView extends BaseSceneView<SceneRouteStationChargeServiceBinding, SceneRouteGasStationChargeServiceImpl> {

    private Hashtable<String, ISceneRouteGasStationChargeSelectCallBack> sceneRouteGasStationWeatherServiceSelectCallBack;

    public SceneRouteGasStationChargeServiceView(@NonNull Context context) {
        super(context);
    }

    public SceneRouteGasStationChargeServiceView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneRouteGasStationChargeServiceView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected SceneRouteStationChargeServiceBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
        return SceneRouteStationChargeServiceBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneRouteGasStationChargeServiceImpl initSceneImpl() {
        sceneRouteGasStationWeatherServiceSelectCallBack = new Hashtable<>();
        return new SceneRouteGasStationChargeServiceImpl(this);
    }

    public void registerRouteSelectObserver(String key, ISceneRouteGasStationChargeSelectCallBack callBack) {
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

    public void clickTab(int tabIndex) {
        for (ISceneRouteGasStationChargeSelectCallBack callBack : sceneRouteGasStationWeatherServiceSelectCallBack.values()) {
            if (ConvertUtils.isEmpty(callBack)) continue;
            callBack.onTabListGasChargeClick(tabIndex);
        }
    }

    public void highlightAlongTab() {
        mScreenViewModel.isGasStationSelect = true;
        mScreenViewModel.isWeatherSelect = false;
        mScreenViewModel.isServiceSelect = false;
        updateUi();
    }
}
