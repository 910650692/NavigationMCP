package com.fy.navi.hmi.route.newAlterCharge;

import android.content.Context;
import android.os.Bundle;
import android.view.View;


import androidx.recyclerview.widget.LinearLayoutManager;

import com.alibaba.android.arouter.facade.annotation.Route;
import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentNewAlterChargeBinding;
import com.fy.navi.hmi.route.NewAlterChargeViewModel;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.scene.ui.adapter.RouteReplaceSupplementAdapter;
import com.fy.navi.scene.ui.adapter.RouteSupplementAdapter;
import com.fy.navi.scene.ui.search.RouteSearchLoadingDialog;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.route.RouteAlterChargeStationInfo;
import com.fy.navi.service.define.route.RouteAlterChargeStationParam;
import com.fy.navi.service.define.route.RouteChargeStationDetailInfo;
import com.fy.navi.service.define.route.RouteSupplementInfo;
import com.fy.navi.service.define.route.RouteSupplementParams;
import com.fy.navi.service.define.search.ChargeInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.ui.action.ViewAdapterKt;
import com.fy.navi.ui.base.BaseFragment;

import java.util.ArrayList;

/**
 * @author  LiuChang
 * @version  \$Revision.1.0\$
 * Date: 2025/3/6
 * Description: [替换补充能点界面]
 */
@Route(path = RoutePath.Route.NEW_ALTER_CHARGE_FRAGMENT)
public class NewAlterChargeFragment extends BaseFragment<FragmentNewAlterChargeBinding, NewAlterChargeViewModel> {
    private static final String TAG = "NewAlterChargeFragment";
    private PoiInfoEntity mDetailPoiInfoEntity;
    private RouteSupplementAdapter mAdapter;
    private RouteReplaceSupplementAdapter mCurrentAlterAdapter;
    private ArrayList<RouteSupplementInfo> mRouteSupplementInfos;
    private RouteSearchLoadingDialog mSearchLoadingDialog;


    @Override
    public int onLayoutId() {
        return R.layout.fragment_new_alter_charge;
    }

    @Override
    public int onInitVariableId() {
        return BR.viewModel;
    }

    @Override
    public void onInitView() {
        mAdapter = new RouteSupplementAdapter();
        mBinding.rvSupplement.setLayoutManager(new LinearLayoutManager(requireContext()));
        mBinding.rvSupplement.setAdapter(mAdapter);
        mAdapter.setItemClickListener(new RouteSupplementAdapter.OnItemClickListener() {
            @Override
            public void onExpandClick(RouteReplaceSupplementAdapter routeReplaceSupplementAdapter,
                                      ArrayList<RouteAlterChargeStationInfo> routeAlterChargeStationInfos) {
                if (routeAlterChargeStationInfos != null || !routeAlterChargeStationInfos.isEmpty()) {
                    ArrayList<String> pidList = new ArrayList<>();
                    for (RouteAlterChargeStationInfo routeAlterChargeStationInfo : routeAlterChargeStationInfos) {
                        pidList.add(routeAlterChargeStationInfo.getMPoiId());
                    }
                    showSearchProgressUI();
                    mViewModel.getAlterPoiListSearch(pidList);
                }
                mCurrentAlterAdapter = routeReplaceSupplementAdapter;
            }

            @Override
            public void onItemClick(PoiInfoEntity newPoiInfoEntity, PoiInfoEntity oldPoiInfoEntity) {
                mViewModel.replaceSupplement(newPoiInfoEntity, oldPoiInfoEntity);
            }
        });
    }

    @Override
    public void onInitData() {
        onInitClick();
    }

    @Override
    public void onGetFragmentData() {
        final Bundle bundle = getArguments();
        if (bundle == null) {
            return;
        }

        final RouteSupplementParams routeSupplementParams = (RouteSupplementParams)
                bundle.getSerializable(AutoMapConstant.RouteBundleKey.BUNDLE_KEY_SUPPLEMENT);
        final RouteSupplementInfo routeChargeStationDetailInfo = (RouteSupplementInfo)
                bundle.getSerializable(AutoMapConstant.RouteBundleKey.BUNDLE_KEY_ALTER_CHARGE_STATION);
        if (routeChargeStationDetailInfo != null && routeSupplementParams != null) {
            mViewModel.setCurrentRouteSupplementParams(routeSupplementParams);
            showSupplementDetails(routeChargeStationDetailInfo);
            getSupplementList(routeSupplementParams);
        }else if (routeSupplementParams != null) {
            mViewModel.setCurrentRouteSupplementParams(routeSupplementParams);
            mViewModel.getShowAlterCharge().set(true);
            getSupplementList(routeSupplementParams);
        }

    }

    @Override
    public void onReStoreFragment() {
        mViewModel.reStoreFragment();
    }

    @Override
    protected void onNewIntent(final Bundle bundle) {
        super.onNewIntent(bundle);
        final RouteSupplementInfo routeChargeStationDetailInfo = (RouteSupplementInfo)
                bundle.getSerializable(AutoMapConstant.RouteBundleKey.BUNDLE_KEY_ALTER_CHARGE_STATION);
        showSupplementDetails(routeChargeStationDetailInfo);

    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        if (!ConvertUtils.isEmpty(mSearchLoadingDialog)) {
            mSearchLoadingDialog.dismiss();
            mSearchLoadingDialog = null;
        }
    }

    /**
     * 设置补能点详情数据
     * @param routeChargeStationDetailInfo 参数
     */
    private void showSupplementDetails(final RouteSupplementInfo routeChargeStationDetailInfo) {
        if (routeChargeStationDetailInfo == null) {
            Logger.d(TAG, "routeChargeStationDetailInfo is null");
            return;
        }

        mViewModel.getShowAlterCharge().set(false);
        mViewModel.getRouteCurrentName().set(routeChargeStationDetailInfo.getMName());
        mViewModel.getCurrentDetails(routeChargeStationDetailInfo.getMPoiID());
    }

    /**
     * 设置补能点详情数据
     * @param routeSupplementParams 参数
     */
    public void getSupplementList(final RouteSupplementParams routeSupplementParams) {
        if (routeSupplementParams == null) {
            Logger.d(TAG, "routeChargeStationDetailInfo is null");
            return;
        }
        final ArrayList<RouteSupplementInfo> routeSupplementInfos = routeSupplementParams.getMRouteSupplementInfos();
        if (routeSupplementInfos == null || routeSupplementInfos.isEmpty()) {
            return;
        }
        final ArrayList<String> pidList = new ArrayList<>();
        for (RouteSupplementInfo routeSupplementInfo : routeSupplementInfos) {
            pidList.add(routeSupplementInfo.getMPoiID());
        }

        if (!pidList.isEmpty()) {
            mViewModel.clearAlterChargeStation();
            for (int i = 0 ; i < pidList.size(); i++) {
                mViewModel.requestAlterChargeStation(pidList.get(i), i);
            }
            mViewModel.getPoiListSearch(pidList);
            mRouteSupplementInfos = routeSupplementParams.getMRouteSupplementInfos();
        }

    }
    /**
     * 点击事件初始话
     */
    public void onInitClick() {
        mBinding.stlAlter.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(final View view) {
                mViewModel.getCloseDetail().call();
            }
        });
    }

    /**
     * 显示充电站列表信息
     * @param routeAlterChargeStationParam 替换充电站搜索信息
     * @param index index
     */
    public void showAlterChargeStationInfo(final RouteAlterChargeStationParam routeAlterChargeStationParam, final int index) {
        if (mAdapter != null) {
            mAdapter.setAlterChargeStation(routeAlterChargeStationParam, index);
        }
    }

    /**
     * 显示替换充电站详情界面
     * @param poiInfoEntities 点参数
     */
    public void showChargeStationDetail(final PoiInfoEntity poiInfoEntities) {
        if (poiInfoEntities == null) {
            Logger.d(TAG, "poiInfoEntities is null");
            return;
        }
        mDetailPoiInfoEntity = poiInfoEntities;
        ThreadManager.getInstance().postUi(() -> {
            mViewModel.getShowAlterCharge().set(false);
            mViewModel.getRouteSearchName().set(poiInfoEntities.getName());
            mViewModel.getRouteSearchAddress().set(poiInfoEntities.getAddress());
            ViewAdapterKt.loadImageUrl(mBinding.scenePoiDetailsChargingStationView.poiChargeImg,
                    poiInfoEntities.getImageUrl(), com.fy.navi.scene.R.drawable.test_pic, com.fy.navi.scene.R.drawable.test_pic);
            if (!ConvertUtils.isEmpty(poiInfoEntities.getServiceAreaInfoList())
                    && !poiInfoEntities.getServiceAreaInfoList().isEmpty()
                    && !ConvertUtils.isEmpty(poiInfoEntities.getServiceAreaInfoList().get(0))) {
                final int building = poiInfoEntities.getServiceAreaInfoList().get(0).getBuilding();
                mViewModel.getRouteSearchStatusVisibility().set(true);
                switch (building) {
                    case 0:
                        mViewModel.getRouteSearchStatusVisibility().set(false);
                        break;
                    case 1:
                        mViewModel.getRouteSearchStatus().set(ResourceUtils.Companion.getInstance().getString(R.string.route_details_building));
                        break;
                    case 2:
                        mViewModel.getRouteSearchStatus().set(ResourceUtils.Companion.getInstance().getString(R.string.route_details_not_find));
                        break;
                    case 3:
                        mViewModel.getRouteSearchStatus().set(ResourceUtils.Companion.getInstance().getString(R.string.route_details_starting));
                        break;
                    case 4:
                        mViewModel.getRouteSearchStatus().set(ResourceUtils.Companion.getInstance().getString(R.string.route_details_stoped));
                        break;
                    default:
                        break;
                }
            }
            if (ConvertUtils.isEmpty(poiInfoEntities.getPhone())) {
                mBinding.scenePoiDetailsChargingStationView.poiChargeAreaPhone.setVisibility(View.GONE);
                mBinding.stlFunction.setVisibility(View.GONE);
            } else {
                mBinding.scenePoiDetailsChargingStationView.poiChargeAreaPhone.setVisibility(View.VISIBLE);
                mBinding.stlFunction.setVisibility(View.VISIBLE);
                mBinding.scenePoiDetailsChargingStationView.poiChargeAreaPhone
                        .setText(ResourceUtils.Companion.getInstance().getString(R.string.route_poi_details_phone) + poiInfoEntities.getPhone());
            }

            if (!ConvertUtils.isEmpty(poiInfoEntities.getChargeInfoList()) && !poiInfoEntities.getChargeInfoList().isEmpty()) {
                final ChargeInfo chargeInfo = poiInfoEntities.getChargeInfoList().get(0);
                mViewModel.getRouteSearchTypeVisibility().set(2);
                if (chargeInfo.getSlowVolt() == 0 && chargeInfo.getSlowPower() == 0
                        && chargeInfo.getSlow_free() == 0 && chargeInfo.getSlow_total() == 0) {
                    mBinding.scenePoiDetailsChargingStationView.poiChargeSlowLayout.setVisibility(View.GONE);
                } else {
                    mBinding.scenePoiDetailsChargingStationView.poiChargeSlowLayout.
                            setVisibility(View.VISIBLE);
                }
                if (chargeInfo.getFastVolt() == 0 && chargeInfo.getFastPower() == 0
                        && chargeInfo.getFast_free() == 0 && chargeInfo.getFast_total() == 0) {
                    mBinding.scenePoiDetailsChargingStationView.poiChargeFastLayout.setVisibility(View.GONE);
                } else {
                    mBinding.scenePoiDetailsChargingStationView.poiChargeFastLayout.
                            setVisibility(View.VISIBLE);
                }

                mBinding.scenePoiDetailsChargingStationView.poiChargeFastOccupied.setText(String.valueOf(chargeInfo.getFast_free()));
                mBinding.scenePoiDetailsChargingStationView.poiChargeFastTotal.setText(ResourceUtils.Companion.getInstance()
                        .getString(R.string.route_details_jg) + chargeInfo.getFast_total());
                mBinding.scenePoiDetailsChargingStationView.poiChargeFastCurrentAndVlot.setText(getString(R.string.route_charge_info_format
                        , chargeInfo.getFastPower() , chargeInfo.getFastVolt()));
                mBinding.scenePoiDetailsChargingStationView.poiChargeSlowOccupied.setText(String.valueOf(chargeInfo.getSlow_free()));
                mBinding.scenePoiDetailsChargingStationView.poiChargeSlowTotal.setText(ResourceUtils.Companion.getInstance()
                        .getString(R.string.route_details_jg) + chargeInfo.getSlow_total());
                mBinding.scenePoiDetailsChargingStationView.poiChargeSlowCurrentAndVlot
                        .setText(getString(R.string.route_charge_info_format, chargeInfo.getSlowPower(), chargeInfo.getSlowVolt()));
                mBinding.scenePoiDetailsChargingStationView.poiChargePrice
                        .setText(ResourceUtils.Companion.getInstance().getString(R.string.route_details_charge_free)
                                + chargeInfo.getCurrentElePrice()
                                + ResourceUtils.Companion.getInstance().getString(R.string.route_details_charge_free_unit));

                mBinding.scenePoiDetailsChargingStationView.poiChargeParkPrice
                        .setText(ResourceUtils.Companion.getInstance().getString(R.string.route_details_charge_park_free)
                                +chargeInfo.getCurrentServicePrice());
            }

        });
    }

    /***
     * 展示POI详情的剩余电量数据
     * @param leftCharge 剩余电量
     */
    public void showPOIDetailCharge(final int leftCharge) {
        if (!ConvertUtils.isEmpty(leftCharge)) {
            //50%以上电量，显示满电量图片，20-50%电量，显示半电量图片
            //0-20电量，显示低电量图片，文本变红
            //小于0%电量，显示空电量图片，文本变红
            if (leftCharge >= 50 && leftCharge <= 100) {
                mBinding.sivArrivalCapacity.setImageResource(com.fy.navi.scene.R.drawable.img_electricity_full_42);
                mBinding.poiArrivalCapacity.setTextColor(
                        ResourceUtils.Companion.getInstance().getColor(com.fy.navi.scene.R.color.text_color_route_item_select));
            } else if (leftCharge > 20 && leftCharge < 50) {
                mBinding.sivArrivalCapacity.setImageResource(com.fy.navi.scene.R.drawable.img_electricity_medium_42);
                mBinding.poiArrivalCapacity.setTextColor(
                        ResourceUtils.Companion.getInstance().getColor(com.fy.navi.scene.R.color.text_color_route_item_select));
            } else if (leftCharge > 0 && leftCharge <= 20) {
                mBinding.sivArrivalCapacity.setImageResource(com.fy.navi.scene.R.drawable.img_electricity_low_42);
                mBinding.poiArrivalCapacity.setTextColor(
                        ResourceUtils.Companion.getInstance().getColor(com.fy.navi.scene.R.color.search_color_delete_bg));
            } else if (leftCharge <= 0) {
                mBinding.sivArrivalCapacity.setImageResource(com.fy.navi.scene.R.drawable.img_electricity_empty_42);
                mBinding.poiArrivalCapacity.setTextColor(
                        ResourceUtils.Companion.getInstance().getColor(com.fy.navi.scene.R.color.search_color_delete_bg));
            }
        }
    }

    public void setSilentSearchResult(final ArrayList<PoiInfoEntity> poiInfoEntities) {
        if (poiInfoEntities == null || poiInfoEntities.isEmpty()) {
            return;
        }

        if (mRouteSupplementInfos == null || mRouteSupplementInfos.isEmpty()) {
            return;
        }
        mAdapter.setRouteSupplementInfos(mRouteSupplementInfos, poiInfoEntities);
    }

    public void setAlterSilentSearchResult(final ArrayList<PoiInfoEntity> poiInfoEntities) {
        hideSearchProgressUI();
        if (poiInfoEntities == null || poiInfoEntities.isEmpty() || mCurrentAlterAdapter == null) {
            return;
        }
        mCurrentAlterAdapter.setPoiInfoEntities(poiInfoEntities);
    }

    /***
     * 搜索请求弹框开启
     */
    public void showSearchProgressUI() {
        if (!ConvertUtils.isEmpty(mSearchLoadingDialog) && mSearchLoadingDialog.isShowing()) {
            Logger.d("mSearchLoadingDialog is showing");
            return;
        }
        final Context context = this.getContext();
        if (context == null) {
            return;
        }
        if (isAdded() && getActivity() != null && !getActivity().isFinishing()) {
            mSearchLoadingDialog = new RouteSearchLoadingDialog(context);
            if (!ConvertUtils.isEmpty(mSearchLoadingDialog)) {
                mSearchLoadingDialog.show();
            }
        }
    }

    /***
     * 搜索请求弹框关闭
     */
    public void hideSearchProgressUI() {
        if (!ConvertUtils.isEmpty(mSearchLoadingDialog)) {
            mSearchLoadingDialog.dismiss();
            mSearchLoadingDialog = null;
        }
    }
}
