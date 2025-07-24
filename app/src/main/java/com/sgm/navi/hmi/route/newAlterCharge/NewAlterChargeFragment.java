package com.sgm.navi.hmi.route.newAlterCharge;

import android.animation.ValueAnimator;
import android.content.Context;
import android.os.Bundle;
import android.view.View;
import android.view.animation.LinearInterpolator;


import androidx.recyclerview.widget.LinearLayoutManager;

import com.alibaba.android.arouter.facade.annotation.Route;
import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.hmi.BR;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.FragmentNewAlterChargeBinding;
import com.sgm.navi.hmi.route.NewAlterChargeViewModel;
import com.sgm.navi.scene.RoutePath;
import com.sgm.navi.scene.ui.adapter.RouteReplaceSupplementAdapter;
import com.sgm.navi.scene.ui.adapter.RouteSupplementAdapter;
import com.sgm.navi.scene.ui.search.RouteSearchLoadingDialog;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.define.mapdata.CityDataInfo;
import com.sgm.navi.service.define.route.RouteAlterChargeStationInfo;
import com.sgm.navi.service.define.route.RouteAlterChargeStationParam;
import com.sgm.navi.service.define.route.RouteSupplementInfo;
import com.sgm.navi.service.define.route.RouteSupplementParams;
import com.sgm.navi.service.define.search.ChargeInfo;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.ui.action.ViewAdapterKt;
import com.sgm.navi.ui.base.BaseFragment;

import java.util.ArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

/**
 * @author  LiuChang
 * @version  \$Revision.1.0\$
 * Date: 2025/3/6
 * Description: [替换补充能点界面]
 */
@Route(path = RoutePath.Route.NEW_ALTER_CHARGE_FRAGMENT)
public class NewAlterChargeFragment extends BaseFragment<FragmentNewAlterChargeBinding, NewAlterChargeViewModel> {
    private static final String TAG = "NewAlterChargeFragment";
    private RouteSupplementAdapter mAdapter;
    private RouteReplaceSupplementAdapter mCurrentAlterAdapter;
    private ArrayList<RouteSupplementInfo> mRouteSupplementInfos;
    private RouteSearchLoadingDialog mSearchLoadingDialog;
    private ValueAnimator mAnimator;
    private float mAngelTemp = 0;
    private CountDownLatch mRequestAlterLock;
    private boolean isAutoExpand = false;
    private final Runnable mTimeoutTask = new Runnable() {
        @Override
        public void run() {
            showLoadingFail();
        }
    };


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
        initLoadAnim(mBinding.ivLoading);
        mAdapter.setItemClickListener(new RouteSupplementAdapter.OnItemClickListener() {
            @Override
            public void onExpandClick(RouteReplaceSupplementAdapter routeReplaceSupplementAdapter,
                                      ArrayList<RouteAlterChargeStationInfo> routeAlterChargeStationInfos) {
                ThreadManager.getInstance().postUi(() -> {
                    if (routeAlterChargeStationInfos != null && !routeAlterChargeStationInfos.isEmpty()) {
                        ArrayList<String> pidList = new ArrayList<>();
                        for (RouteAlterChargeStationInfo routeAlterChargeStationInfo : routeAlterChargeStationInfos) {
                            pidList.add(routeAlterChargeStationInfo.getMPoiId());
                        }
                        showSearchProgressUI();
                        mViewModel.getAlterPoiListSearch(pidList);
                    }
                    mCurrentAlterAdapter = routeReplaceSupplementAdapter;
                });
            }

            @Override
            public void onItemClick(PoiInfoEntity newPoiInfoEntity, PoiInfoEntity oldPoiInfoEntity) {
                mViewModel.replaceSupplement(newPoiInfoEntity, oldPoiInfoEntity);
            }

            @Override
            public void onAlterShowClick(ArrayList<RouteAlterChargeStationInfo> routeAlterChargeStationInfos) {
                if (routeAlterChargeStationInfos == null || routeAlterChargeStationInfos.isEmpty()) {
                    mViewModel.clearLayerItem();
                } else {
                    mViewModel.updateRouteReplaceChargePoints(routeAlterChargeStationInfos);
                }
            }

            @Override
            public void onItemDetailsClick(PoiInfoEntity newPoiInfoEntity, PoiInfoEntity oldPoiInfoEntity) {
                mViewModel.setCloseFragment(false);
                showAlterSupplementDetails(newPoiInfoEntity, oldPoiInfoEntity);
            }

            @Override
            public void onDetailsClick(RouteSupplementInfo routeSupplementInfo, int position) {
                mViewModel.setCurrentIndex(position);
                mViewModel.setCloseFragment(false);
                mViewModel.setSearchDetail(true);
                isAutoExpand = true;
                mAdapter.setCurrentSelectedIndex(position);
                showSupplementDetails(routeSupplementInfo);
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
                bundle.getParcelable(AutoMapConstant.RouteBundleKey.BUNDLE_KEY_SUPPLEMENT);
        final RouteSupplementInfo routeChargeStationDetailInfo = (RouteSupplementInfo)
                bundle.getParcelable(AutoMapConstant.RouteBundleKey.BUNDLE_KEY_ALTER_CHARGE_STATION);
        if (routeChargeStationDetailInfo != null && routeSupplementParams != null) {
            mViewModel.setSearchDetail(true);
            showSupplementDetails(routeChargeStationDetailInfo);
            mViewModel.setRouteSupplementParams(routeSupplementParams);
            mViewModel.setCurrentIndex(routeSupplementParams.getMRouteSupplementInfos().indexOf(routeChargeStationDetailInfo));
        }else if (routeSupplementParams != null) {
            mViewModel.setSearchDetail(false);
            mViewModel.setRouteSupplementParams(routeSupplementParams);
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
                bundle.getParcelable(AutoMapConstant.RouteBundleKey.BUNDLE_KEY_ALTER_CHARGE_STATION);
        mViewModel.setSearchDetail(true);
        showSupplementDetails(routeChargeStationDetailInfo);

    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        if (!ConvertUtils.isEmpty(mSearchLoadingDialog)) {
            mSearchLoadingDialog.dismiss();
            mSearchLoadingDialog = null;
        }
        if (mRequestAlterLock != null) {
            mRequestAlterLock = null;
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

        showLoading(true);
        mViewModel.getRouteCurrentName().set(routeChargeStationDetailInfo.getMName());
        mViewModel.setAlterButton(null, null, false);
        mViewModel.setSearchPoi(routeChargeStationDetailInfo.getMPoiID());
        mViewModel.getCurrentDetails(routeChargeStationDetailInfo.getMPoiID());
    }

    /**
     * 设置替换补能点详情数据
     * @param newPoiInfoEntity 替换充电站
     * @param oldPoiInfoEntity 被替换充电站
     */
    private void showAlterSupplementDetails(final PoiInfoEntity newPoiInfoEntity, final PoiInfoEntity oldPoiInfoEntity) {
        if (newPoiInfoEntity == null || oldPoiInfoEntity == null) {
            Logger.d(TAG, "PoiInfoEntity is null");
            return;
        }

        showLoading(true);
        mViewModel.setSearchDetail(true);
        mViewModel.setSearchPoi(newPoiInfoEntity.getPid());
        mViewModel.getRouteCurrentName().set(newPoiInfoEntity.getMName());
        mViewModel.setAlterButton(newPoiInfoEntity, oldPoiInfoEntity, true);
        mViewModel.getCurrentDetails(newPoiInfoEntity.getPid());
    }

    /**
     * 设置补能点详情数据
     * @param routeSupplementParams 参数
     */
    public void getSupplementList(final RouteSupplementParams routeSupplementParams) {
        showLoading(true);
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
            mRequestAlterLock = new CountDownLatch(pidList.size());
            mViewModel.clearAlterChargeStation();
            for (int i = 0 ; i < pidList.size(); i++) {
                mViewModel.requestAlterChargeStation(pidList.get(i), i);
            }
            mViewModel.getPoiListSearch(pidList);
            mRouteSupplementInfos = routeSupplementParams.getMRouteSupplementInfos();
            ThreadManager.getInstance().execute(() -> {
                try {
                    boolean completed = mRequestAlterLock.await(10, TimeUnit.SECONDS);
                    if (!completed) {
                        // 超时处理
                        Logger.e(TAG, "Request alter lock timeout");
                        return;
                    }
                } catch (InterruptedException e) {
                    Logger.e(TAG, e);
                }
                autoExpand();
            });
        }
    }

    /**
     * 自动展开查看的备选充电站
     */
    public void autoExpand() {
        ThreadManager.getInstance().postDelay(() -> {
            if (mAdapter != null && mCurrentAlterAdapter == null && mBinding != null && getContext() != null) {
                mBinding.rvSupplement.post(() -> {
                    int currentIndex = mViewModel.getCurrentIndex();
                    if (currentIndex != -1) {
                        Logger.d(TAG, "autoExpand currentIndex:" + currentIndex);
                        LinearLayoutManager layoutManager = (LinearLayoutManager) mBinding.rvSupplement.getLayoutManager();
                        if (layoutManager != null) {
                            layoutManager.scrollToPositionWithOffset(currentIndex, 0);
                        }
                        RouteAlterChargeStationParam routeAlterChargeStationParam = mAdapter.getAlterChargeStation(currentIndex);
                        if (routeAlterChargeStationParam == null ||
                                routeAlterChargeStationParam.getMRouteAlterChargeStationInfos() == null ||
                                routeAlterChargeStationParam.getMRouteAlterChargeStationInfos().isEmpty()) {
                            mViewModel.setCurrentIndex(-1);
                        } else {
                            isAutoExpand = true;
                            mAdapter.setCurrentSelectedIndex(currentIndex);
                        }
                    }
                });
            } else {
                Logger.d(TAG, "autoExpand fail");
                mViewModel.setCurrentIndex(-1);
            }
        }, 500);
    }

    /**
     * 点击事件初始话
     */
    public void onInitClick() {
        mBinding.stlAlter.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(final View view) {
                if (mViewModel.getAlterButton()) {
                    mViewModel.detailReplaceClick();
                } else {
                    mViewModel.getCloseDetail().call();
                }
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
        if (mRequestAlterLock != null) {
            mRequestAlterLock.countDown();
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
        ThreadManager.getInstance().postUi(() -> {
            mViewModel.getRouteSearchName().set(poiInfoEntities.getName());
            mViewModel.getRouteSearchAddress().set(poiInfoEntities.getAddress());
            ViewAdapterKt.loadImageUrl(mBinding.scenePoiDetailsChargingStationView.poiChargeImg,
                    poiInfoEntities.getImageUrl(), com.sgm.navi.scene.R.drawable.test_pic, com.sgm.navi.scene.R.drawable.test_pic);
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
                mBinding.scenePoiDetailsChargingStationView.poiChargeFastCurrentAndVlot.setText(ResourceUtils.Companion.getInstance()
                        .getString(R.string.route_charge_info_format
                        , chargeInfo.getFastPower() , chargeInfo.getFastVolt()));
                mBinding.scenePoiDetailsChargingStationView.poiChargeSlowOccupied.setText(String.valueOf(chargeInfo.getSlow_free()));
                mBinding.scenePoiDetailsChargingStationView.poiChargeSlowTotal.setText(ResourceUtils.Companion.getInstance()
                        .getString(R.string.route_details_jg) + chargeInfo.getSlow_total());
                mBinding.scenePoiDetailsChargingStationView.poiChargeSlowCurrentAndVlot
                        .setText(ResourceUtils.Companion.getInstance().getString(R.string.route_charge_info_format,
                                chargeInfo.getSlowPower(), chargeInfo.getSlowVolt()));
                if (chargeInfo.getCurrentElePrice() == null || chargeInfo.getCurrentElePrice().equals("--")) {
                    mBinding.scenePoiDetailsChargingStationView.poiChargePrice.setVisibility(View.GONE);
                } else {
                    mBinding.scenePoiDetailsChargingStationView.poiChargePrice.setVisibility(View.VISIBLE);
                    mBinding.scenePoiDetailsChargingStationView.poiChargePrice
                            .setText(ResourceUtils.Companion.getInstance().getString(R.string.route_details_charge_free)
                                    + chargeInfo.getCurrentElePrice()
                                    + ResourceUtils.Companion.getInstance().getString(R.string.route_details_charge_free_unit));
                }

                if (chargeInfo.getCurrentServicePrice() == null || chargeInfo.getCurrentServicePrice().isEmpty()) {
                    mBinding.scenePoiDetailsChargingStationView.poiChargeParkPrice.setVisibility(View.GONE);
                } else {
                    mBinding.scenePoiDetailsChargingStationView.poiChargeParkPrice.setVisibility(View.VISIBLE);
                    mBinding.scenePoiDetailsChargingStationView.poiChargeParkPrice
                            .setText(ResourceUtils.Companion.getInstance().getString(R.string.route_details_charge_park_free)
                                    +chargeInfo.getCurrentServicePrice());
                }
            }

            if (mViewModel.getAlterButton()) {
                mBinding.stvAlter.setText(ResourceUtils.Companion.getInstance().getString(R.string.route_alter));
            } else {
                mBinding.stvAlter.setText(ResourceUtils.Companion.getInstance().getString(R.string.route_check_alter));
            }

            if (mViewModel.isSearchDetail()) {
                mViewModel.getShowAlterChargeType().set(2);
                ThreadManager.getInstance().removeHandleTask(mTimeoutTask);
            }
        });
    }

    /***
     * 展示POI详情的剩余电量数据
     * @param leftCharge 剩余电量
     */
    public void showPOIDetailCharge(final int leftCharge) {
        if (!ConvertUtils.isEmpty(leftCharge)) {
            mBinding.sivArrivalCapacity.setVisibility(View.VISIBLE);
            //50%以上电量，显示满电量图片，20-50%电量，显示半电量图片
            //0-20电量，显示低电量图片，文本变红
            //小于0%电量，显示空电量图片，文本变红
            if (leftCharge >= 50 && leftCharge <= 100) {
                mBinding.sivArrivalCapacity.setImageResource(com.sgm.navi.scene.R.drawable.img_electricity_full_42);
                mBinding.poiArrivalCapacity.setTextColor(
                        ResourceUtils.Companion.getInstance().getColor(com.sgm.navi.scene.R.color.text_color_route_item_select));
            } else if (leftCharge > 20 && leftCharge < 50) {
                mBinding.sivArrivalCapacity.setImageResource(com.sgm.navi.scene.R.drawable.img_electricity_medium_42);
                mBinding.poiArrivalCapacity.setTextColor(
                        ResourceUtils.Companion.getInstance().getColor(com.sgm.navi.scene.R.color.text_color_route_item_select));
            } else if (leftCharge > 0 && leftCharge <= 20) {
                mBinding.sivArrivalCapacity.setImageResource(com.sgm.navi.scene.R.drawable.img_electricity_low_42);
                mBinding.poiArrivalCapacity.setTextColor(
                        ResourceUtils.Companion.getInstance().getColor(com.sgm.navi.scene.R.color.search_color_delete_bg));
            } else if (leftCharge <= 0) {
                mBinding.sivArrivalCapacity.setImageResource(com.sgm.navi.scene.R.drawable.img_electricity_empty_42);
                mBinding.poiArrivalCapacity.setTextColor(
                        ResourceUtils.Companion.getInstance().getColor(com.sgm.navi.scene.R.color.search_color_delete_bg));
                int chargeTime = (int) Math.abs(leftCharge)/100 + 1;
                mViewModel.getRouteSearchElec().set(ResourceUtils.Companion.getInstance().getString(
                        com.sgm.navi.scene.R.string.remain_charge_travel, chargeTime));
            }
        }
    }

    public void setSilentSearchResult(final ArrayList<PoiInfoEntity> poiInfoEntities) {
        if (!mViewModel.isSearchDetail()) {
            mViewModel.getShowAlterChargeType().set(1);
            ThreadManager.getInstance().removeHandleTask(mTimeoutTask);
        }
        if (poiInfoEntities == null || poiInfoEntities.isEmpty()) {
            return;
        }

        if (mRouteSupplementInfos == null || mRouteSupplementInfos.isEmpty()) {
            return;
        }
        mAdapter.setRouteSupplementInfos(mRouteSupplementInfos, poiInfoEntities);
    }

    public void setAlterSilentSearchResult(final ArrayList<PoiInfoEntity> poiInfoEntities) {
        if (poiInfoEntities == null || poiInfoEntities.isEmpty() || mCurrentAlterAdapter == null) {
            return;
        }
        mCurrentAlterAdapter.setPoiInfoEntities(poiInfoEntities);
        int currentIndex = mViewModel.getCurrentIndex();
        if (currentIndex != -1 && isAutoExpand) {
            ThreadManager.getInstance().postDelay(() -> {
                if (mBinding == null && getContext() == null) {
                    return;
                }
                mBinding.rvSupplement.post(() -> {
                    Logger.d(TAG, "setAlterSilentSearchResult currentIndex:" + currentIndex);
                    LinearLayoutManager layoutManager = (LinearLayoutManager) mBinding.rvSupplement.getLayoutManager();
                    if (layoutManager != null) {
                        layoutManager.scrollToPositionWithOffset(currentIndex, 0);
                    }
                    isAutoExpand = false;
                    mViewModel.setCurrentIndex(-1);
                });
                hideSearchProgressUI();
            }, 500);
        } else {
            hideSearchProgressUI();
        }
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
            if (mSearchLoadingDialog == null) {
                mSearchLoadingDialog = new RouteSearchLoadingDialog(context);
            }
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

    /**
     * 初始化加载动画
     * @param sivLoading 加载动画视图
     */
    private void initLoadAnim(final View sivLoading) {
        // 如果动画已存在并正在运行，则取消并清理
        if (mAnimator != null) {
            if (mAnimator.isRunning()) {
                mAnimator.cancel();
            }
            mAnimator = null;
        }

        // 创建属性动画，从 0 到 360 度循环旋转
        mAnimator = ValueAnimator.ofFloat(0f, 360f);
        mAnimator.setDuration(2000); // 动画持续时间
        mAnimator.setRepeatCount(ValueAnimator.INFINITE); // 无限重复
        mAnimator.setInterpolator(new LinearInterpolator()); // 线性插值器
        // 添加动画更新监听器
        mAnimator.addUpdateListener(animation -> {
            final float angle = (float) animation.getAnimatedValue();
            if (shouldSkipUpdate(angle)) {
                return;
            }
            sivLoading.setRotation(angle);
        });
    }

    /**
     *用于控制角度变化频率的辅助方法
     *@param angle 当前角度
     *@return 是否跳过更新
     */
    private boolean shouldSkipUpdate(final float angle) {
        final float changeAngle = angle - mAngelTemp;
        final float angleStep = 10;
        if (changeAngle > 0f && changeAngle <= angleStep) {
            return true; // 跳过更新，避免高频调用浪费资源
        }
        mAngelTemp = angle; // 更新临时角度值
        return false;
    }

    /**
     * 是否显示加载动画
     * @param isShow 是否显示
     */
    public void showLoading(final boolean isShow) {
        ThreadManager.getInstance().postUi(() -> {
            mViewModel.getShowAlterChargeType().set(3);
            ThreadManager.getInstance().removeHandleTask(mTimeoutTask);
            ThreadManager.getInstance().postDelay(mTimeoutTask, 8000);
            mBinding.ivLoading.setVisibility(isShow ? View.VISIBLE : View.GONE);
            mBinding.noResultHint.setText(ResourceUtils.Companion.getInstance().getString(com.sgm.navi.scene.R.string.address_loading));
            mBinding.noResultButton.setVisibility(View.GONE);
            if (mAnimator != null) {
                if (isShow) {
                    mAnimator.start();
                } else {
                    mAnimator.cancel();
                }
            }
        });
    }


    public void showLoadingFail() {
        if (!ConvertUtils.isEmpty(mBinding) && getContext() != null) {
            mViewModel.getShowAlterChargeType().set(3);
            mBinding.noResultButton.setVisibility(View.VISIBLE);
            mBinding.noResultHint.setText(ResourceUtils.Companion.getInstance().getString(com.sgm.navi.scene.R.string.load_failed));
            mBinding.ivLoading.setVisibility(View.GONE);
            if (mAnimator != null) {
                mAnimator.cancel();
            }
            mBinding.noResultButton.setOnClickListener((view) -> {
                if (mViewModel.isSearchDetail() && mViewModel.getSearchPoi() != null) {
                    showLoading(true);
                    mViewModel.getCurrentDetails(mViewModel.getSearchPoi());
                } else if (!mViewModel.isSearchDetail() && mViewModel.getRouteSupplementParams() != null) {
                    if (mAdapter != null) {
                        mAdapter.clearAlterChargeStation();
                    }
                    getSupplementList(mViewModel.getRouteSupplementParams());
                }
            });
        }
    }
}
