package com.sgm.navi.scene.ui.route;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;


import com.android.utils.ConvertUtils;
import com.sgm.navi.scene.BaseSceneView;
import com.sgm.navi.scene.api.route.ISceneRouteGasStationWeatherServiceSelectCallBack;
import com.sgm.navi.scene.databinding.SceneRouteStationWeatherServiceBinding;
import com.sgm.navi.scene.impl.route.SceneRouteGasStationWeatherServiceImpl;

import java.util.Hashtable;

public class SceneRouteGasStationWeatherServiceView extends BaseSceneView<SceneRouteStationWeatherServiceBinding
        , SceneRouteGasStationWeatherServiceImpl> {

    private Hashtable<String, ISceneRouteGasStationWeatherServiceSelectCallBack> mSceneRouteGasStationWeatherServiceSelectCallBack;

    public SceneRouteGasStationWeatherServiceView(final Context context) {
        super(context);
    }

    public SceneRouteGasStationWeatherServiceView(final Context context, final AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneRouteGasStationWeatherServiceView(final Context context, final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected SceneRouteStationWeatherServiceBinding createViewBinding(final LayoutInflater inflater, final ViewGroup viewGroup) {
        return SceneRouteStationWeatherServiceBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneRouteGasStationWeatherServiceImpl initSceneImpl() {
        mSceneRouteGasStationWeatherServiceSelectCallBack = new Hashtable<>();
        return new SceneRouteGasStationWeatherServiceImpl(this);
    }
    /**
     * fragment注册监听
     * @param key 关键字
     * @param callBack 回调
     * */
    public void registerRouteSelectObserver(final String key, final ISceneRouteGasStationWeatherServiceSelectCallBack callBack) {
        mSceneRouteGasStationWeatherServiceSelectCallBack.put(key, callBack);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setScene(mScreenViewModel);
    }

    @Override
    protected void initObserver() {
    }
    /**
     * 更新Tab
     * */
    public void updateUi() {
        if (ConvertUtils.isEmpty(mViewBinding) || ConvertUtils.isEmpty(mScreenViewModel)) {
            return;
        }
        mViewBinding.routeRightTabListTvChargingStation.setSelected(mScreenViewModel.isChargingStationSelect());
        mViewBinding.routeRightTabListIvChargingStation.setSelected(mScreenViewModel.isChargingStationSelect());
        mViewBinding.routeRightTabListTvGasStation.setSelected(mScreenViewModel.isGasStationSelect());
        mViewBinding.routeRightTabListIvGasStation.setSelected(mScreenViewModel.isGasStationSelect());
        mViewBinding.routeRightTabListTvWeather.setSelected(mScreenViewModel.isWeatherSelect());
        mViewBinding.routeRightTabListIvWeather.setSelected(mScreenViewModel.isWeatherSelect());
        mViewBinding.routeRightTabListTvService.setSelected(mScreenViewModel.isServiceSelect());
        mViewBinding.routeRightTabListIvService.setSelected(mScreenViewModel.isServiceSelect());
    }
    /**
     * 取消Tab的状态
     * */
    public void clearSceneTabUI() {
        if (mScreenViewModel != null) {
            mScreenViewModel.setChargingStationSelect(false);
            mScreenViewModel.setWeatherSelect(false);
            mScreenViewModel.setServiceSelect(false);
            mScreenViewModel.setGasStationSelect(false);
            updateUi();
        }
    }

    /**
     * 设置车辆动力类型
     * @param carType 动力类型
     * */
    public void setCarType(final int carType) {
        if (ConvertUtils.isEmpty(mViewBinding) || ConvertUtils.isEmpty(mScreenViewModel)) {
            return;
        }
        if (carType == 1) {
            mViewBinding.routeRightTabListGasStation.setVisibility(View.GONE);
            mViewBinding.routeRightTabListChargingStation.setVisibility(View.VISIBLE);
        } else if (carType == 0) {
            mViewBinding.routeRightTabListGasStation.setVisibility(View.VISIBLE);
            mViewBinding.routeRightTabListChargingStation.setVisibility(View.GONE);
        } else if (carType == 2) {
            mViewBinding.routeRightTabListGasStation.setVisibility(View.VISIBLE);
            mViewBinding.routeRightTabListChargingStation.setVisibility(View.VISIBLE);
        }
    }

    /**
     * 回调Tab的点击
     * @param tabIndex tab 索引
     * @param isChceked tab 是否选中
     * */
    public void clickTab(final int tabIndex, final boolean isChceked) {
        for (ISceneRouteGasStationWeatherServiceSelectCallBack callBack : mSceneRouteGasStationWeatherServiceSelectCallBack.values()) {
            if (ConvertUtils.isEmpty(callBack)) {
                continue;
            }
            callBack.onTabListClick(tabIndex, isChceked);
        }
    }
}
