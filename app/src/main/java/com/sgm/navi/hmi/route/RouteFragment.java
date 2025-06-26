package com.sgm.navi.hmi.route;

import android.annotation.SuppressLint;
import android.content.Context;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;
import android.view.ViewStub;

import androidx.annotation.NonNull;
import androidx.constraintlayout.widget.ConstraintLayout;
import androidx.constraintlayout.widget.ConstraintSet;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.ItemTouchHelper;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.alibaba.android.arouter.facade.annotation.Route;
import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.burypoint.anno.HookMethod;
import com.sgm.navi.burypoint.bean.BuryProperty;
import com.sgm.navi.burypoint.constant.BuryConstant;
import com.sgm.navi.burypoint.controller.BuryPointController;
import com.sgm.navi.hmi.BR;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.FragmentRouteBinding;
import com.sgm.navi.hmi.databinding.RoutePoiDetailsPageBinding;
import com.sgm.navi.hmi.databinding.RouteResultDetailsPageBinding;
import com.sgm.navi.hmi.databinding.RouteResultPageBinding;
import com.sgm.navi.hmi.databinding.RouteSearchChargeGasPageBinding;
import com.sgm.navi.hmi.databinding.RouteSearchServicePageBinding;
import com.sgm.navi.hmi.databinding.RouteWeatherDetailsPageBinding;
import com.sgm.navi.scene.RoutePath;
import com.sgm.navi.scene.adapter.GasStationAdapter;
import com.sgm.navi.scene.adapter.GridSpacingItemDecoration;
import com.sgm.navi.scene.databinding.ScenePoiDetailsChargingStationViewBinding;
import com.sgm.navi.scene.databinding.ScenePoiDetailsGasStationViewBinding;
import com.sgm.navi.scene.databinding.ScenePoiDetailsServiceAreaViewBinding;
import com.sgm.navi.scene.dialog.MsgTopDialog;
import com.sgm.navi.scene.impl.imersive.ImersiveStatus;
import com.sgm.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.sgm.navi.scene.ui.adapter.RoutePOIGasStationAdapter;
import com.sgm.navi.scene.ui.adapter.RoutePOIIconAdapter;
import com.sgm.navi.scene.ui.adapter.RouteViaPointAdapter;
import com.sgm.navi.scene.ui.route.SceneRouteDescendantsView;
import com.sgm.navi.scene.ui.route.SceneRouteDetailsResultListView;
import com.sgm.navi.scene.ui.route.SceneRouteGasStationChargeServiceView;
import com.sgm.navi.scene.ui.route.SceneRoutePreferenceView;
import com.sgm.navi.scene.ui.route.SceneRouteResultListView;
import com.sgm.navi.scene.ui.route.SceneRouteSearchChargeRefreshListView;
import com.sgm.navi.scene.ui.route.SceneRouteSearchRefreshListView;
import com.sgm.navi.scene.ui.search.RouteRequestLoadingDialog;
import com.sgm.navi.scene.ui.search.RouteSearchLoadingDialog;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.route.RouteLineInfo;
import com.sgm.navi.service.define.route.RouteLineSegmentInfo;
import com.sgm.navi.service.define.route.RouteMsgPushInfo;
import com.sgm.navi.service.define.route.RoutePageLevel;
import com.sgm.navi.service.define.route.RouteParam;
import com.sgm.navi.service.define.route.RoutePriorityType;
import com.sgm.navi.service.define.route.RouteRequestParam;
import com.sgm.navi.service.define.route.RouteRestAreaDetailsInfo;
import com.sgm.navi.service.define.route.RouteSpeechRequestParam;
import com.sgm.navi.service.define.route.RouteSupplementInfo;
import com.sgm.navi.service.define.route.RouteWayID;
import com.sgm.navi.service.define.search.ChargeInfo;
import com.sgm.navi.service.define.search.GasStationInfo;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.search.ServiceAreaInfo;
import com.sgm.navi.service.define.utils.BevPowerCarUtils;
import com.sgm.navi.service.logicpaket.setting.SettingPackage;
import com.sgm.navi.ui.action.ViewAdapterKt;
import com.sgm.navi.ui.base.BaseFragment;
import com.sgm.navi.ui.define.TripID;
import com.sgm.navi.ui.dialog.IBaseDialogClickListener;
import com.sgm.navi.ui.view.SkinConstraintLayout;
import com.sgm.navi.ui.view.SkinImageView;
import com.sgm.navi.ui.view.SkinLinearLayout;
import com.sgm.navi.ui.view.SkinRecyclerView;
import com.sgm.navi.ui.view.SkinSwitch;
import com.sgm.navi.ui.view.SkinTextView;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

@Route(path = RoutePath.Route.ROUTE_FRAGMENT)
public class RouteFragment extends BaseFragment<FragmentRouteBinding, RouteViewModel> {
    private static final String TAG = RouteFragment.class.getSimpleName();
    private final int mSpacing = 24; // 上下间距
    private final int mHorizontalSpacing = 32; // 左右间距
    private final int mSpanCount = 2;//数据列数
    private final int mMaxWidthWithBatter= 322;
    private final int mMaxWidthWithoutBatter= 530;
    private static final int SWIPE_THRESHOLD = 100;
    private static final int TIPS_DELAY = 8000;
    private RouteRequestLoadingDialog mRouteRequestLoadingDialog;
    private RouteSearchLoadingDialog mSearchLoadingDialog;
    private RouteViaPointAdapter mRouteViaPointAdapter;
    private RoutePOIIconAdapter mPoiIconAdapter;
    private RoutePOIGasStationAdapter mGasStationAdapter;
    private MsgTopDialog mMsgTopDialog;
    private Map<PoiInfoEntity, View> mRouteChargeProgressViews;
    private float mStartY;
    private float mEndY;

    private final static String ROUTE_ERROR = "异常";
    private ViewStub mRouteListPageStub;
    private ViewStub mRouteDetailsListPageStub;
    private ViewStub mRouteServiceListPageStub;
    private ViewStub mRouteChargeGasListPageStub;
    private ViewStub mRouteWeatherPageStub;
    private ViewStub mRoutePoiDetailsPageStub;

    private RouteResultPageBinding mRouteListPageView;
    private RouteResultDetailsPageBinding mRouteDetailsListPageView;
    private RouteSearchServicePageBinding mRouteServiceListPageView;
    private RouteSearchChargeGasPageBinding mRouteChargeGasListPageView;
    private RouteWeatherDetailsPageBinding mRouteWeatherPageView;
    private RoutePoiDetailsPageBinding mRoutePoiDetailsPageView;

    @Override
    public int onLayoutId() {
        return R.layout.fragment_route;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {
        initTabPage();
        changePage(RoutePageLevel.ROUTE_RESULT_LIST);
    }



    //------------右侧Tab***************************************************/
    /**
     *
     *Tab初始化
     * **/
    @SuppressLint("ClickableViewAccessibility")
    private void initTabPage() {
        //设置scene
        mBinding.routeRightTabListScene.setScreenId(MapType.valueOf(mScreenId));
        mBinding.routeRightTabListScene.registerRouteSelectObserver(TAG, mViewModel);

        //设置监听
        mBinding.routeRightTabListScene.setOnTouchListener(mViewModel);

        //设置car type
        mBinding.routeRightTabListScene.setCarType(mViewModel.powerType());
    }

    /***
     * 去除Tab选中
     */
    public void clearSceneTabUI() {
        if (!ConvertUtils.isEmpty(mBinding) && !ConvertUtils.isEmpty(mBinding.routeRightTabListScene)) {
            mBinding.routeRightTabListScene.clearSceneTabUI();
        } else {
            Logger.e(TAG, ROUTE_ERROR);
        }
    }

    /***
     * 去除Tab选中
     * @param isCharging 是否是充电
     */
    public void clearSceneTabUI(final boolean isCharging) {
        if (!ConvertUtils.isEmpty(mBinding) && !ConvertUtils.isEmpty(mBinding.routeRightTabListTvChargingStation)
                && !ConvertUtils.isEmpty(mBinding.routeRightTabListIvChargingStation)) {
            mBinding.routeRightTabListTvChargingStation.setSelected(isCharging);
            mBinding.routeRightTabListIvChargingStation.setSelected(isCharging);
        } else {
            Logger.e(TAG, ROUTE_ERROR);
        }
    }

    /***
     * 去除GasTab选中
     * @param isGas 是否是加油
     */
    public void clearSceneGasTabUI(final boolean isGas) {
        if (!ConvertUtils.isEmpty(mBinding) && !ConvertUtils.isEmpty(mBinding.routeRightTabListTvGasStation)
                && !ConvertUtils.isEmpty(mBinding.routeRightTabListIvGasStation)) {
            mBinding.routeRightTabListTvGasStation.setSelected(isGas);
            mBinding.routeRightTabListIvGasStation.setSelected(isGas);
        } else {
            Logger.e(TAG, ROUTE_ERROR);
        }
    }

    //------------右侧Tab***************************************************/


    //------------路线列表***************************************************/
    /**
     *
     *路线列表页面初始化
     * **/
    @SuppressLint("ClickableViewAccessibility")
    private void initRouteResultPage() {
        if (null == mRouteListPageView) {
            Logger.e(TAG, "mRouteListPageView = null");
            return;
        }
        //设置viewModel
        mRouteListPageView.setViewModel(mViewModel);
        SkinSwitch routeLineInfoSwitchEnergy = mRouteListPageView.routeLineInfoSwitchEnergy;

        //补能能规划
        routeLineInfoSwitchEnergy.setChecked(SettingPackage.getInstance().getChargingPlan());
        routeLineInfoSwitchEnergy.setOnCheckedChangeListener((compoundButton, b) -> {
            mViewModel.cancelTimer();
            if (b) {
                hideTrip();
            }
            BevPowerCarUtils.getInstance().isElecPlanRoute = b;
            SettingPackage.getInstance().setChargingPlan(b);
            final RouteRequestParam param = new RouteRequestParam();
            param.setMRouteWay(RouteWayID.ROUTE_WAY_REFRESH);
            param.setMRoutePriorityType(RoutePriorityType.ROUTE_TYPE_MANUAL_REFRESH);
            mViewModel.setCurrentEnergy();
            mViewModel.requestRoute(param);
        });
        BevPowerCarUtils.getInstance().isElecPlanRoute = routeLineInfoSwitchEnergy.isChecked();

        //途经点列表
        SkinRecyclerView routeLineViaPoiRecycle = mRouteListPageView.routeLineViaPoiRecycle;
        final LinearLayoutManager layoutManager = new LinearLayoutManager(getContext());
        layoutManager.setOrientation(LinearLayoutManager.VERTICAL);
        routeLineViaPoiRecycle.setLayoutManager(layoutManager);
        mRouteViaPointAdapter = new RouteViaPointAdapter();
        mRouteViaPointAdapter.setDeleteViaPointListener(index -> {
            mViewModel.cancelTimer();
            mViewModel.deleteViaParamMode(index);
        });
        routeLineViaPoiRecycle.setAdapter(mRouteViaPointAdapter);
        final ItemTouchHelper touchHelper = new ItemTouchHelper(new ItemTouchHelper.Callback() {
            private int mOriginalPosition = -1;
            private int movePosition = -1;

            @Override
            public int getMovementFlags(@NonNull final RecyclerView recyclerView, @NonNull final RecyclerView.ViewHolder viewHolder) {
                final int dragFlags = ItemTouchHelper.UP | ItemTouchHelper.DOWN;
                final int swipeFlags = ItemTouchHelper.START | ItemTouchHelper.END;
                return makeMovementFlags(dragFlags, swipeFlags);
            }

            @Override
            public boolean onMove(@NonNull final RecyclerView recyclerView, @NonNull final RecyclerView.ViewHolder viewHolder
                    , final RecyclerView.ViewHolder target) {
                if (mOriginalPosition == -1) {
                    mOriginalPosition = viewHolder.getAdapterPosition();
                }
                movePosition = target.getAdapterPosition();
                mRouteViaPointAdapter.onItemMove(viewHolder.getAdapterPosition(), target.getAdapterPosition());
                return true;
            }

            @Override
            public void onSwiped(@NonNull final RecyclerView.ViewHolder viewHolder, final int direction) {
            }

            @Override
            public void clearView(@NonNull final RecyclerView recyclerView, @NonNull final RecyclerView.ViewHolder viewHolder) {
                super.clearView(recyclerView, viewHolder);
                if (mOriginalPosition == movePosition) {
                    Logger.d(TAG, "The position has not changed");
                    mOriginalPosition = -1;
                    return;
                }
                mViewModel.changeParamListMode(mOriginalPosition, movePosition);
                mOriginalPosition = -1;
            }

            @Override
            public boolean isItemViewSwipeEnabled() {
                return false;
            }
        });
        touchHelper.attachToRecyclerView(routeLineViaPoiRecycle);

        //途径点滑动
        mRouteListPageView.routeLineInfoTitle.setOnTouchListener((v, event) -> {
            switch (event.getAction()) {
                case MotionEvent.ACTION_DOWN:
                    mStartY = event.getY();
                    break;
                case MotionEvent.ACTION_UP:
                    mEndY = event.getY();
                    final float diffY = mEndY - mStartY;
                    if (Math.abs(diffY) > SWIPE_THRESHOLD) {
                        mViewModel.getOpenCloseViaClick().call();
                    }
                    break;
                default:
                    break;
            }
            return true;
        });

        //路线列表
        SceneRouteResultListView routeLineInfoSceneRouteResult = mRouteListPageView.routeLineInfoSceneRouteResult;
        routeLineInfoSceneRouteResult.setScreenId(MapType.valueOf(mScreenId));
        routeLineInfoSceneRouteResult.registerRouteSelectObserver(TAG, mViewModel);

        //偏好数据
        SceneRoutePreferenceView routeLineInfoSceneRoutePerference = mRouteListPageView.routeLineInfoSceneRoutePerference;
        routeLineInfoSceneRoutePerference.setScreenId(MapType.valueOf(mScreenId));
        routeLineInfoSceneRoutePerference.registerRoutePreferenceObserver(TAG, mViewModel);

        //防点击穿透
        mRouteListPageView.getRoot().setOnTouchListener(mViewModel);
        routeLineViaPoiRecycle.setOnTouchListener(mViewModel);

        //子poi
        mRouteListPageView.lySecondaryPoi.setItemClickListener(new SceneRouteDescendantsView.OnItemClickListener() {
            @Override
            public void onItemClick(final PoiInfoEntity poiInfo) {
                mViewModel.cancelTimer();
                mViewModel.requestChangeEnd(poiInfo);
            }

            @Override
            public void onCancelSelectClick(final PoiInfoEntity poiInfoEntity) {
                mViewModel.cancelTimer();
                mViewModel.requestChangeEnd(poiInfoEntity);
            }

            @Override
            public void OnScrollListener() {
                mViewModel.cancelTimer();
            }
        });
    }

    /***
     * 渲染算路结果列表
     * @param routeLineInfos 数据
     */
    @HookMethod(eventName = BuryConstant.EventName.AMAP_ROUTE_LIST)
    public void setRouteResultListUI(final List<RouteLineInfo> routeLineInfos) {
        if (!ConvertUtils.isEmpty(mRouteListPageView) && !ConvertUtils.isEmpty(mRouteListPageView.routeLineInfoSceneRouteResult)) {
            mRouteListPageView.routeLineInfoSceneRouteResult.notifyResultList(routeLineInfos);
        } else {
            Logger.e(TAG, ROUTE_ERROR);
        }

        //For Bury Point
        final BuryProperty buryProperty = new BuryProperty.Builder().setParams(BuryConstant.ProperType.BURY_KEY_HOME_PREDICTION,
                Integer.toString(routeLineInfos.size())).build();
        BuryPointController.getInstance().setBuryProps(buryProperty);
    }

    /***
     * 渲染子poi显示界面
     * @param type type
     * @param poiInfoEntity 数据
     */
    public void setRouteSecondaryPoiUI(final int type, final PoiInfoEntity poiInfoEntity) {
        if (null == mRouteListPageView) {
            Logger.e(TAG, "mRouteListPageView = null");
            return;
        }
        mRouteListPageView.lySecondaryPoi.setUIMode(type, poiInfoEntity);
    }

    /***
     * 设置补能规划开关
     */
    public void setEnergyChecked() {
        if (null == mRouteListPageView) {
            Logger.e(TAG, "mRouteListPageView = null");
            return;
        }
        if (!ConvertUtils.isEmpty(mBinding) && !ConvertUtils.isEmpty(mRouteListPageView.routeLineInfoSwitchEnergy)) {
            mRouteListPageView.routeLineInfoSwitchEnergy.setChecked(!mRouteListPageView.routeLineInfoSwitchEnergy.isChecked());
        } else {
            Logger.e(TAG, ROUTE_ERROR);
        }
    }

    /***
     * 获取补能规划开关
     *
     * @return 开关状态
     */
    public boolean getEnergyChecked() {
        if (null == mRouteListPageView) {
            Logger.e(TAG, "mRouteListPageView = null");
            return false;
        }
        if (!ConvertUtils.isEmpty(mBinding) && !ConvertUtils.isEmpty(mRouteListPageView.routeLineInfoSwitchEnergy)) {
            return !mRouteListPageView.routeLineInfoSwitchEnergy.isChecked() &&
                    mRouteListPageView.routeLineInfoSwitchEnergy.getVisibility() == View.VISIBLE;
        } else {
            Logger.e(TAG, ROUTE_ERROR);
            return false;
        }
    }

    /***
     * 开启Trip弹框
     * @param title 标题
     * @param content 内容
     */
    public void showTripDialog(final String title, final String content) {
        if (null == mRouteListPageView) {
            Logger.e(TAG, "mRouteListPageView = null");
            return;
        }
        if (ConvertUtils.isEmpty(mBinding) || ConvertUtils.isEmpty(mRouteListPageView.routeLineInfoSwitchEnergy)
                || ConvertUtils.isEmpty(mViewModel)) {
            Logger.e(TAG, ROUTE_ERROR);
            return;
        }
        if (mRouteListPageView.routeLineInfoSwitchEnergy.isChecked()) {
            return;
        }
        if (!ConvertUtils.isEmpty(mMsgTopDialog) && mMsgTopDialog.isShowing()) {
            return;
        }
        mViewModel.cancelTimer();
        final Context context = this.getContext();
        if (context == null) {
            return;
        }
        if (isAdded() && getActivity() != null && !getActivity().isFinishing()) {
            mMsgTopDialog = new MsgTopDialog(context, TripID.ROUTE_LOW_BATTER,
                    mViewModel.getMsgDialogTop(), mViewModel.getMsgDialogLeft());
            mMsgTopDialog.setTitle(title);
            mMsgTopDialog.setContent(content);
            mMsgTopDialog.setDialogClickListener(new IBaseDialogClickListener() {
                @Override
                public void onCommitClick(final TripID tripID) {
                    if (tripID == TripID.ROUTE_LOW_BATTER) {
                        mRouteListPageView.routeLineInfoSwitchEnergy.setChecked(true);
                    }
                }
            });
            mMsgTopDialog.showDialog();
            ThreadManager.getInstance().postDelay(this::hideTrip, TIPS_DELAY);
        }
    }

    /***
     * 更新选中路线
     * @param routeIndex 选中路线索引
     */
    public void updateSelectRouteUI(final int routeIndex) {
        if (null == mRouteListPageView) {
            Logger.e(TAG, "mRouteListPageView = null");
            return;
        }
        if (!ConvertUtils.isEmpty(mBinding) && !ConvertUtils.isEmpty(mRouteListPageView.routeLineInfoSceneRouteResult)) {
            mRouteListPageView.routeLineInfoSceneRouteResult.updateSelectRouteUI(routeIndex);
        } else {
            Logger.e(TAG, ROUTE_ERROR);
        }
    }

    /***
     * 添加主屏补能规划界面
     */
    public void updateSupplementPointsView(final ArrayList<RouteSupplementInfo> routeSupplementInfos, final float total) {
        if (null == mRouteListPageView) {
            Logger.e(TAG, "mRouteListPageView = null");
            return;
        }
        mRouteListPageView.routeRouteSupplementPoints.updateSupplementPointsView(routeSupplementInfos, total);
    }

    /***
     * 切换算路偏好最大宽度
     * @param batter 是否补能
     */
    public void setPreferenceMaxWidth(final boolean batter) {
        if (null == mRouteListPageView) {
            Logger.e(TAG, "mRouteListPageView = null");
            return;
        }
        ThreadManager.getInstance().postUi(() -> {
            if (batter) {
                mRouteListPageView.routeLineInfoTvPrefer.setMaxWidth(mMaxWidthWithBatter);
            } else {
                mRouteListPageView.routeLineInfoTvPrefer.setMaxWidth(mMaxWidthWithoutBatter);
            }
        });
    }


    /***
     * 设置途经点列表
     * @param routeParams 数据
     */
    public void setViaList(final List<RouteParam> routeParams) {
        if (!ConvertUtils.isEmpty(mRouteViaPointAdapter)) {
            mRouteViaPointAdapter.setRouteBeanList(routeParams);
        } else {
            Logger.e(TAG, ROUTE_ERROR);
        }
    }

    /**
     *
     *凯迪MFC专用，不可删除
     * **/
    public View getRootViewForMFC() {
        if (null == mRouteListPageView) {
            Logger.e(TAG, "mRouteListPageView = null");
            return null;
        }
        return mRouteListPageView.routeLineInfoBgRouteStartNavi;
    }

    //------------路线列表***************************************************/




    //------------路线详情***************************************************/
    /**
     *
     * 初始化路线详情
     * */
    private void initRouteDetailsPage() {
        //设置viewmodel
        mRouteDetailsListPageView.setViewModel(mViewModel);

        //设置路线详情列表
        SceneRouteDetailsResultListView routeDetailInfoSceneRouteDetailsResult = mRouteDetailsListPageView.routeDetailInfoSceneRouteDetailsResult;
        routeDetailInfoSceneRouteDetailsResult.setScreenId(MapType.valueOf(mScreenId));
        routeDetailInfoSceneRouteDetailsResult.registerRouteDeatailsCheckedObserver(TAG, mViewModel);

        //设置监听
        mRouteDetailsListPageView.getRoot().setOnTouchListener(mViewModel);
    }

    public void setRouteDetailsEndName(String endName) {
        if (null == mRouteDetailsListPageView) {
            Logger.e(TAG, "mRouteDetailsListPageView = null");
            return;
        }
        mRouteDetailsListPageView.routeDetailInfoSceneRouteDetailsResult.setEndPoint(endName);
    }

    /***
     * 设置规避UI
     * @param isAvoid true
     */
    public void setAvoidStatusUI(final boolean isAvoid) {
        if (null == mRouteDetailsListPageView) {
            Logger.e(TAG, "mRouteDetailsListPageView = null");
            return;
        }
        if (!ConvertUtils.isEmpty(mBinding) && !ConvertUtils.isEmpty(mRouteDetailsListPageView.routeDetailInfoSceneRouteDetailsResult)) {
            mRouteDetailsListPageView.routeDetailInfoSceneRouteDetailsResult.setAvoidStatus(isAvoid);
        } else {
            Logger.e(TAG, ROUTE_ERROR);
        }
    }

    /***
     * 点击规避
     */
    public void startAvoidRoad() {
        if (null == mRouteDetailsListPageView) {
            Logger.e(TAG, "mRouteDetailsListPageView = null");
            return;
        }
        if (!ConvertUtils.isEmpty(mViewModel)) {
            mViewModel.getRouteDetailsVisibility().set(true);
        } else {
            Logger.e(TAG, ROUTE_ERROR);
        }
        setAvoidStatusUI(false);
        if (!ConvertUtils.isEmpty(mBinding) && !ConvertUtils.isEmpty(mRouteDetailsListPageView.routeDetailInfoSceneRouteDetailsResult)) {
            mRouteDetailsListPageView.routeDetailInfoSceneRouteDetailsResult.startAvoidRoad();
        } else {
            Logger.e(TAG, ROUTE_ERROR);
        }
    }



    /***
     * 渲染导航段
     * @param routeLineSegmentInfos 数据
     */
    public void setDetailsResult(final List<RouteLineSegmentInfo> routeLineSegmentInfos) {
        if (null == mRouteDetailsListPageView) {
            Logger.e(TAG, "mRouteDetailsListPageView = null");
            return;
        }
        if (!ConvertUtils.isEmpty(mBinding) && !ConvertUtils.isEmpty(mRouteDetailsListPageView.routeDetailInfoSceneRouteDetailsResult)) {
            mRouteDetailsListPageView.routeDetailInfoSceneRouteDetailsResult.notifyRouteDetailsResultList(routeLineSegmentInfos);
        } else {
            Logger.e(TAG, ROUTE_ERROR);
        }
    }
    //------------路线详情***************************************************/

    //------------服务区列表***************************************************/
    /**
     *
     * 初始化服务区列表页面
     * */
    public void initServiceListPage() {
        //设置viewmodel
        mRouteServiceListPageView.setViewModel(mViewModel);

        //设置scene
        SceneRouteSearchRefreshListView routeDetailInfoSceneRouteSearchRefresh = mRouteServiceListPageView.routeDetailInfoSceneRouteSearchRefresh;
        routeDetailInfoSceneRouteSearchRefresh.setScreenId(MapType.valueOf(mScreenId));
        routeDetailInfoSceneRouteSearchRefresh.registerRouteSearchRefreshObserver(TAG, mViewModel);
    }

    /***
     * 展示服务区列表
     * @param poiInfoEntities 数据
     */
    public void showRouteSearchListUI(final List<RouteRestAreaDetailsInfo> poiInfoEntities) {
        if (null == mRouteServiceListPageView) {
            Logger.e(TAG, "mRouteServiceListPageView = null");
            return;
        }
        if (!ConvertUtils.isEmpty(mBinding) && !ConvertUtils.isEmpty(mRouteServiceListPageView.routeDetailInfoSceneRouteSearchRefresh)) {
            mRouteServiceListPageView.routeDetailInfoSceneRouteSearchRefresh.notifyResultList(poiInfoEntities);
        } else {
            Logger.e(TAG, ROUTE_ERROR);
        }
    }

    //------------服务区列表***************************************************/

    //------------充电&加油列表***************************************************/
    /**
     *
     * 初始化服务区列表页面
     * */
    public void initChargeGasListPage() {
        //设置viewmodel
        mRouteChargeGasListPageView.setViewModel(mViewModel);

        //设置scene
        SceneRouteGasStationChargeServiceView routeRightTabListChargeScene = mRouteChargeGasListPageView.routeRightTabListChargeScene;
        routeRightTabListChargeScene.setScreenId(MapType.valueOf(mScreenId));
        routeRightTabListChargeScene.registerRouteSelectObserver(TAG, mViewModel);
        SceneRouteSearchChargeRefreshListView routeChargeInfoSceneRouteSearchRefresh = mRouteChargeGasListPageView.routeChargeInfoSceneRouteSearchRefresh;
        routeChargeInfoSceneRouteSearchRefresh.setScreenId(MapType.valueOf(mScreenId));
        routeChargeInfoSceneRouteSearchRefresh.registerRouteSearchRefreshObserver(TAG, mViewModel);

    }

    /**
     * 设置终点图片
     *
     */
    public void setSearchCharge(boolean isCharge) {
        mRouteChargeGasListPageView.routeRightTabListChargeScene.setSearchCharge(isCharge);
    }

    /***
     * 展示充电站列表
     * @param poiInfoEntities 搜索数据
     * @param gasChargeAlongList 本地已添加数据
     * @param searchType 搜索方式 0-沿途搜索
     * @param type 列表类别 0:充电站 1：加油站
     */
    public void showRouteSearchChargeListUI(final List<PoiInfoEntity> poiInfoEntities
            , final List<RouteParam> gasChargeAlongList, final int searchType, final int type) {
        if (null == mRouteChargeGasListPageView) {
            Logger.e(TAG, "mRouteChargeGasListPageView = null");
            return;
        }
        if (!ConvertUtils.isEmpty(mBinding) && !ConvertUtils.isEmpty(mRouteChargeGasListPageView.routeChargeInfoSceneRouteSearchRefresh)) {
            mRouteChargeGasListPageView.routeChargeInfoSceneRouteSearchRefresh.notifyResultList(poiInfoEntities, gasChargeAlongList, searchType, type);
        } else {
            Logger.e(TAG, ROUTE_ERROR);
        }
    }

    /***
     * 高亮沿途按钮
     */
    public void highlightAlongTab() {
        if (null == mRouteChargeGasListPageView) {
            Logger.e(TAG, "mRouteChargeGasListPageView = null");
            return;
        }
        if (!ConvertUtils.isEmpty(mBinding) && !ConvertUtils.isEmpty(mRouteChargeGasListPageView.routeRightTabListChargeScene)) {
            mRouteChargeGasListPageView.routeRightTabListChargeScene.highlightAlongTab();
        } else {
            Logger.e(TAG, ROUTE_ERROR);
        }
    }

    /***
     * 更新充电本地数据
     * @param gasChargeAlongList 本地数据
     * @param listSearchType 搜索方式
     */
    public void updateChargeList(final List<RouteParam> gasChargeAlongList, final int listSearchType) {
        if (null == mRouteChargeGasListPageView) {
            Logger.e(TAG, "mRouteChargeGasListPageView = null");
            return;
        }
        if (!ConvertUtils.isEmpty(mBinding) && !ConvertUtils.isEmpty(mRouteChargeGasListPageView.routeChargeInfoSceneRouteSearchRefresh)) {
            mRouteChargeGasListPageView.routeChargeInfoSceneRouteSearchRefresh.updateChargeList(gasChargeAlongList, listSearchType);
        } else {
            Logger.e(TAG, ROUTE_ERROR);
        }
    }

    /***
     * 更新路径
     * @param progress 百分比
     */
    public void updateRouteChargeExhaustUi(final float progress) {
        if (null == mRouteChargeGasListPageView) {
            Logger.e(TAG, "mRouteChargeGasListPageView = null");
            return;
        }
        if (ConvertUtils.isEmpty(mBinding) || ConvertUtils.isEmpty(mRouteChargeGasListPageView.routeChargeExhaustionPoint)) {
            Logger.e(TAG, ROUTE_ERROR);
            return;
        }
        ThreadManager.getInstance().postUi(() -> {
            final ConstraintLayout.LayoutParams layoutParams = (ConstraintLayout.LayoutParams) mRouteChargeGasListPageView.routeChargeExhaustionPoint.getLayoutParams();
            layoutParams.horizontalBias = progress;
            mRouteChargeGasListPageView.routeChargeExhaustionPoint.setLayoutParams(layoutParams);
        });
    }

    /***
     * 设置充电UI
     * @param poiInfoEntity POI数据
     * @param progress 百分比
     */
    public void addRouteChargePoiUi(final PoiInfoEntity poiInfoEntity, final float progress) {
        if (null == mRouteChargeGasListPageView) {
            Logger.e(TAG, "mRouteChargeGasListPageView = null");
            return;
        }
        if (ConvertUtils.isEmpty(mBinding) || ConvertUtils.isEmpty(mRouteChargeGasListPageView.routeChargeProgressIcons)) {
            Logger.e(TAG, ROUTE_ERROR);
            return;
        }
        ThreadManager.getInstance().postUi(() -> {
            final SkinConstraintLayout routeChargeProgressLayout = mRouteChargeGasListPageView.routeChargeProgressIcons;
            final LayoutInflater inflater = getLayoutInflater();
            final View customViewItem = inflater.inflate(R.layout.item_route_charge_progress, routeChargeProgressLayout, false);
            customViewItem.setId(View.generateViewId());
            final SkinTextView distanceText = customViewItem.findViewById(R.id.tv_route_charge);
            distanceText.setText(poiInfoEntity.getDistance().replace("公里", "km").replace("米","m"));
            if (mRouteChargeProgressViews == null) {
                mRouteChargeProgressViews = new ConcurrentHashMap<>();
            }
            routeChargeProgressLayout.addView(customViewItem);

            final ConstraintSet constraintSet = new ConstraintSet();
            constraintSet.clone(routeChargeProgressLayout);
            constraintSet.connect(customViewItem.getId(), ConstraintSet.START, routeChargeProgressLayout.getId(), ConstraintSet.START);
            constraintSet.connect(customViewItem.getId(), ConstraintSet.END, routeChargeProgressLayout.getId(), ConstraintSet.END);
            constraintSet.connect(customViewItem.getId(), ConstraintSet.TOP, routeChargeProgressLayout.getId(), ConstraintSet.TOP);
            constraintSet.setHorizontalBias(customViewItem.getId(), progress);
            constraintSet.applyTo(routeChargeProgressLayout);
            mRouteChargeProgressViews.put(poiInfoEntity, customViewItem);

        });

    }

    /***
     * 移除充电UI
     * @param poiInfoEntity POI数据
     */
    public void removeRouteChargePoiUi(final PoiInfoEntity poiInfoEntity) {
        if (null == mRouteChargeGasListPageView) {
            Logger.e(TAG, "mRouteChargeGasListPageView = null");
            return;
        }
        ThreadManager.getInstance().postUi(() -> {
            if (mRouteChargeProgressViews != null) {
                final View view = mRouteChargeProgressViews.get(poiInfoEntity);
                if (view != null) {
                    mRouteChargeGasListPageView.routeChargeProgressIcons.removeView(view);
                }
                mRouteChargeProgressViews.remove(poiInfoEntity);
            }
        });
    }

    /***
     * 清除所有充电UI
     */
    public void clearRouteChargePoiUi() {
        if (null == mRouteChargeGasListPageView) {
            Logger.e(TAG, "mRouteChargeGasListPageView = null");
            return;
        }
        ThreadManager.getInstance().postUi(() -> {
            if (mRouteChargeProgressViews != null && !mRouteChargeProgressViews.isEmpty()) {
                for (PoiInfoEntity poiInfoEntity : mRouteChargeProgressViews.keySet()) {
                    final View view = mRouteChargeProgressViews.get(poiInfoEntity);
                    if (view != null && !ConvertUtils.isEmpty(mBinding)
                            && !ConvertUtils.isEmpty(mRouteChargeGasListPageView.routeChargeProgressIcons)) {
                        mRouteChargeGasListPageView.routeChargeProgressIcons.removeView(view);
                    }
                    mRouteChargeProgressViews.remove(poiInfoEntity);
                }
            }
        });
    }
    //------------充电&加油列表***************************************************/


    //------------天气详情页面***************************************************/
    /**
     *
     * 初始化服务区列表页面
     * */
    public void initWeatherDetailsPage(){
        //设置viewmodel
        mRouteWeatherPageView.setViewModel(mViewModel);
    }
    //------------天气详情页面***************************************************/



    //------------POI详情页面***************************************************/
    /**
     *
     * 初始化服务区列表页面
     * */
    public void initPoiDetailsPage(){
        //设置viewmodel
        mRoutePoiDetailsPageView.setViewModel(mViewModel);

        //设置scene
        mRoutePoiDetailsPageView.scenePoiDetailsGasStationView.poiGasOilList.addItemDecoration(
                new GridSpacingItemDecoration(getContext(), mSpanCount, mSpacing, mHorizontalSpacing, false));

        //POI详情列表
        final LinearLayoutManager layoutManager = new LinearLayoutManager(getContext());
        layoutManager.setOrientation(LinearLayoutManager.HORIZONTAL);
        SkinRecyclerView poiServiceAreaFacility = mRoutePoiDetailsPageView.scenePoiDetailsServiceAreaView.poiServiceAreaFacility;
        poiServiceAreaFacility.setLayoutManager(layoutManager);
        mPoiIconAdapter = new RoutePOIIconAdapter();
        poiServiceAreaFacility.setAdapter(mPoiIconAdapter);

        final LinearLayoutManager layoutManager1 = new LinearLayoutManager(getContext());
        layoutManager1.setOrientation(LinearLayoutManager.HORIZONTAL);
        SkinRecyclerView routePoidetailGasStation = mRoutePoiDetailsPageView.scenePoiDetailsServiceAreaView.routePoidetailGasStation;
        routePoidetailGasStation.setLayoutManager(layoutManager1);
        mGasStationAdapter = new RoutePOIGasStationAdapter();
        routePoidetailGasStation.setAdapter(mGasStationAdapter);
    }

    /***
     * 展示服务区POI详情数据
     * @param info POI数据
     */
    @SuppressLint("SetTextI18n")
    public void showServiceDetailsUI(final PoiInfoEntity info) {
        if (ConvertUtils.isEmpty(info) || ConvertUtils.isEmpty(mRoutePoiDetailsPageView)
                || ConvertUtils.isEmpty(mRoutePoiDetailsPageView.scenePoiDetailsServiceAreaView)
                || ConvertUtils.isEmpty(mRoutePoiDetailsPageView.scenePoiDetailsServiceAreaView.poiServiceAreaPhone)
                || ConvertUtils.isEmpty(mRoutePoiDetailsPageView.scenePoiDetailsServiceAreaView.poiServiceAreaHours)
                || ConvertUtils.isEmpty(mRoutePoiDetailsPageView.scenePoiDetailsServiceAreaView.poiServiceAreaFacility)
                || ConvertUtils.isEmpty(mRoutePoiDetailsPageView.scenePoiDetailsServiceAreaView.poiServiceAreaOil)
                || ConvertUtils.isEmpty(mRoutePoiDetailsPageView.stlPhone)) {
            Logger.e(TAG, ROUTE_ERROR);
            return;
        }
        ScenePoiDetailsServiceAreaViewBinding scenePoiDetailsServiceAreaView = mRoutePoiDetailsPageView.scenePoiDetailsServiceAreaView;
        SkinTextView poiServiceAreaPhone = scenePoiDetailsServiceAreaView.poiServiceAreaPhone;
        SkinTextView poiServiceAreaHours = scenePoiDetailsServiceAreaView.poiServiceAreaHours;
        SkinRecyclerView poiServiceAreaFacility = scenePoiDetailsServiceAreaView.poiServiceAreaFacility;
        SkinLinearLayout poiServiceAreaOil = scenePoiDetailsServiceAreaView.poiServiceAreaOil;
        SkinConstraintLayout stlPhone = mRoutePoiDetailsPageView.stlPhone;

        if (ConvertUtils.isEmpty(info.getPhone())) {
            poiServiceAreaPhone.setVisibility(View.GONE);
            stlPhone.setVisibility(View.GONE);
        } else {
            poiServiceAreaPhone.setVisibility(View.VISIBLE);
            stlPhone.setVisibility(View.VISIBLE);
            poiServiceAreaPhone
                    .setText(ResourceUtils.Companion.getInstance().getString(R.string.route_poi_details_phone) + info.getPhone());
        }

        if (ConvertUtils.isEmpty(info.getBusinessTime())) {
            poiServiceAreaHours.setVisibility(View.GONE);
        } else {
            poiServiceAreaHours.setVisibility(View.VISIBLE);
            poiServiceAreaHours
                    .setText(ResourceUtils.Companion.getInstance().getString(R.string.route_poi_details_bussTime) + info.getBusinessTime());
        }

        if (!ConvertUtils.isEmpty(info.getServiceAreaInfoList())
                && !info.getServiceAreaInfoList().isEmpty() && !ConvertUtils.isEmpty(info.getServiceAreaInfoList().get(0))
                && !ConvertUtils.isEmpty(mViewModel)) {
            final List<ServiceAreaInfo.ServiceAreaChild> serviceAreaChildList = info.getServiceAreaInfoList().get(0).getServiceAreaChildList();
            final int building = info.getServiceAreaInfoList().get(0).getBuilding();
            mViewModel.getRouteSearchStatusVisibility().set(true);
            switch (building) {
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
                    mViewModel.getRouteSearchStatusVisibility().set(false);
                    break;
            }
            if (!serviceAreaChildList.isEmpty()) {
                poiServiceAreaFacility.setVisibility(View.VISIBLE);
                final List<ServiceAreaInfo.ServiceAreaChild> serviceAreaChildArrayList = new ArrayList<>();
                final List<String> codes = new ArrayList<>();
                for (ServiceAreaInfo.ServiceAreaChild child : serviceAreaChildList) {
                    if (!codes.contains(child.getTypeCode())
                            && isNeed(child.getTypeCode())) {
                        codes.add(child.getTypeCode());
                        serviceAreaChildArrayList.add(child);
                    }
                }
                if (!ConvertUtils.isEmpty(mPoiIconAdapter)) {
                    mPoiIconAdapter.setRouteBeanList(serviceAreaChildArrayList);
                }
                String gasType = "";
                for (ServiceAreaInfo.ServiceAreaChild child : serviceAreaChildArrayList) {
                    if (!ConvertUtils.isEmpty(child.getGasType())) {
                        gasType = child.getGasType();
                    }
                }
                if (ConvertUtils.isEmpty(gasType)) {
                    poiServiceAreaOil.setVisibility(View.GONE);
                } else {
                    final List<String> gasString = new ArrayList<>();
                    if (gasType.contains("|")) {
                        final String[] split = gasType.split("\\|");
                        gasString.addAll(Arrays.asList(split));
                    } else {
                        gasString.add(gasType);
                    }
                    if (!ConvertUtils.isEmpty(mGasStationAdapter)) {
                        mGasStationAdapter.setRouteBeanList(gasString);
                    }
                    poiServiceAreaOil.setVisibility(View.VISIBLE);
                }
            } else {
                poiServiceAreaFacility.setVisibility(View.GONE);
                poiServiceAreaOil.setVisibility(View.GONE);
            }
        }
    }

    /***
     * 需要展示的设施
     * @param typeCode 设置code
     * @return 需要展示的设置code
     */
    private boolean isNeed(final String typeCode) {
        return "010200".equals(typeCode) || "200300".equals(typeCode)
                || "060400".equals(typeCode) || "150904".equals(typeCode)
                || "011100".equals(typeCode) || "010000".equals(typeCode)
                || "050100".equals(typeCode);
    }

    /***
     * 展示充电站POI详情数据
     * @param info POI数据
     */
    @SuppressLint("SetTextI18n")
    public void showChargeDetailsUI(final PoiInfoEntity info) {
        if (null == mRoutePoiDetailsPageView) {
            Logger.e(TAG, "mRoutePoiDetailsPageView = null");
            return;
        }
        mRoutePoiDetailsPageView.sivArrivalCapacity.setVisibility(View.VISIBLE);
        mRoutePoiDetailsPageView.poiArrivalCapacity.setVisibility(View.VISIBLE);
        if (ConvertUtils.isEmpty(info) || ConvertUtils.isEmpty(mRoutePoiDetailsPageView)
                || ConvertUtils.isEmpty(mRoutePoiDetailsPageView.scenePoiDetailsChargingStationView)
                || ConvertUtils.isEmpty(mRoutePoiDetailsPageView.scenePoiDetailsChargingStationView.poiChargeImg)
                || ConvertUtils.isEmpty(mRoutePoiDetailsPageView.scenePoiDetailsChargingStationView.poiChargeAreaPhone)
                || ConvertUtils.isEmpty(mRoutePoiDetailsPageView.scenePoiDetailsChargingStationView.poiCharegBusinessHours)
                || ConvertUtils.isEmpty(mRoutePoiDetailsPageView.scenePoiDetailsChargingStationView.poiChargeFastTotal)
                || ConvertUtils.isEmpty(mRoutePoiDetailsPageView.scenePoiDetailsChargingStationView.poiChargeFastOccupied)
                || ConvertUtils.isEmpty(mRoutePoiDetailsPageView.scenePoiDetailsChargingStationView.poiChargeFastCurrentAndVlot)
                || ConvertUtils.isEmpty(mRoutePoiDetailsPageView.scenePoiDetailsChargingStationView.poiChargeSlowOccupied)
                || ConvertUtils.isEmpty(mRoutePoiDetailsPageView.scenePoiDetailsChargingStationView.poiChargeSlowTotal)
                || ConvertUtils.isEmpty(mRoutePoiDetailsPageView.scenePoiDetailsChargingStationView.poiChargeSlowCurrentAndVlot)
                || ConvertUtils.isEmpty(mRoutePoiDetailsPageView.scenePoiDetailsChargingStationView.poiChargePrice)
                || ConvertUtils.isEmpty(mRoutePoiDetailsPageView.scenePoiDetailsChargingStationView.poiChargeParkPrice)
                || ConvertUtils.isEmpty(mRoutePoiDetailsPageView.stlPhone)) {
            Logger.e(TAG, ROUTE_ERROR);
            return;
        }
        ScenePoiDetailsChargingStationViewBinding scenePoiDetailsChargingStationView = mRoutePoiDetailsPageView.scenePoiDetailsChargingStationView;
        SkinImageView poiChargeImg = scenePoiDetailsChargingStationView.poiChargeImg;
        SkinTextView poiChargeAreaPhone = scenePoiDetailsChargingStationView.poiChargeAreaPhone;
        SkinTextView poiCharegBusinessHours = scenePoiDetailsChargingStationView.poiCharegBusinessHours;
        SkinTextView poiChargeFastTotal = scenePoiDetailsChargingStationView.poiChargeFastTotal;
        SkinTextView poiChargeFastOccupied = scenePoiDetailsChargingStationView.poiChargeFastOccupied;
        SkinTextView poiChargeFastCurrentAndVlot = scenePoiDetailsChargingStationView.poiChargeFastCurrentAndVlot;
        SkinTextView poiChargeSlowOccupied = scenePoiDetailsChargingStationView.poiChargeSlowOccupied;
        SkinTextView poiChargeSlowTotal = scenePoiDetailsChargingStationView.poiChargeSlowTotal;
        SkinTextView poiChargeSlowCurrentAndVlot = scenePoiDetailsChargingStationView.poiChargeSlowCurrentAndVlot;
        SkinTextView poiChargePrice = scenePoiDetailsChargingStationView.poiChargePrice;
        SkinTextView poiChargeParkPrice = scenePoiDetailsChargingStationView.poiChargeParkPrice;


        ViewAdapterKt.loadImageUrl(poiChargeImg
                , info.getImageUrl(), com.sgm.navi.scene.R.drawable.test_pic, com.sgm.navi.scene.R.drawable.test_pic);
        if (ConvertUtils.isEmpty(info.getPhone())) {
            poiChargeAreaPhone.setVisibility(View.GONE);
            mRoutePoiDetailsPageView.stlPhone.setVisibility(View.GONE);
        } else {
            poiChargeAreaPhone.setVisibility(View.VISIBLE);
            mRoutePoiDetailsPageView.stlPhone.setVisibility(View.VISIBLE);
            poiChargeAreaPhone
                    .setText(ResourceUtils.Companion.getInstance().getString(R.string.route_poi_details_phone) + info.getPhone());
        }

        if (ConvertUtils.isEmpty(info.getBusinessTime())) {
            poiCharegBusinessHours.setVisibility(View.GONE);
        } else {
            poiCharegBusinessHours.setVisibility(View.VISIBLE);
            poiCharegBusinessHours
                    .setText(ResourceUtils.Companion.getInstance().getString(R.string.route_poi_details_bussTime) + info.getBusinessTime());
        }
        if (!ConvertUtils.isEmpty(info.getChargeInfoList()) && !info.getChargeInfoList().isEmpty()) {
            final ChargeInfo chargeInfo = info.getChargeInfoList().get(0);
            if (chargeInfo.getSlowVolt() == 0 && chargeInfo.getSlowPower() == 0
                    && chargeInfo.getSlow_free() == 0 && chargeInfo.getSlow_total() == 0) {
                mRoutePoiDetailsPageView.scenePoiDetailsChargingStationView.poiChargeSlowLayout.setVisibility(View.GONE);
            } else {
                mRoutePoiDetailsPageView.scenePoiDetailsChargingStationView.poiChargeSlowLayout.
                        setVisibility(View.VISIBLE);
            }
            if (chargeInfo.getFastVolt() == 0 && chargeInfo.getFastPower() == 0
                    && chargeInfo.getFast_free() == 0 && chargeInfo.getFast_total() == 0) {
                mRoutePoiDetailsPageView.scenePoiDetailsChargingStationView.poiChargeFastLayout.setVisibility(View.GONE);
            } else {
                mRoutePoiDetailsPageView.scenePoiDetailsChargingStationView.poiChargeFastLayout.
                        setVisibility(View.VISIBLE);
            }
            poiChargeFastOccupied.setText(chargeInfo.getFast_free() + "");
            poiChargeFastTotal
                    .setText(ResourceUtils.Companion.getInstance().getString(R.string.route_details_jg) + chargeInfo.getFast_free());
            poiChargeFastCurrentAndVlot
                    .setText(chargeInfo.getFastPower() + "kw." + chargeInfo.getFastVolt() + "v");

            poiChargeSlowOccupied.setText(chargeInfo.getSlow_free() + "");
            poiChargeSlowTotal
                    .setText(ResourceUtils.Companion.getInstance().getString(R.string.route_details_jg) + chargeInfo.getSlow_total());
            poiChargeSlowCurrentAndVlot
                    .setText(chargeInfo.getSlowPower() + "kw." + chargeInfo.getSlowVolt() + "v");

            poiChargePrice
                    .setText(ResourceUtils.Companion.getInstance().getString(R.string.route_details_charge_free)
                            + chargeInfo.getCurrentElePrice()
                            + ResourceUtils.Companion.getInstance().getString(R.string.route_details_charge_free_unit));
            poiChargeParkPrice
                    .setText(ResourceUtils.Companion.getInstance().getString(R.string.route_details_charge_park_free)
                            + chargeInfo.getCurrentServicePrice());
        }
    }

    /***
     * 展示POI详情的剩余电量数据
     * @param leftCharge 剩余电量
     */
    public void showPOIDetailCharge(final int leftCharge) {
        if (!ConvertUtils.isEmpty(leftCharge)) {
            if (ConvertUtils.isEmpty(mRoutePoiDetailsPageView)
                    || ConvertUtils.isEmpty(mRoutePoiDetailsPageView.poiArrivalCapacity)
                    || ConvertUtils.isEmpty(mRoutePoiDetailsPageView.sivArrivalCapacity)) {
                Logger.e(TAG, "mRoutePoiDetailsPageView = null");
                return;
            }
            SkinImageView sivArrivalCapacity = mRoutePoiDetailsPageView.sivArrivalCapacity;
            SkinTextView poiArrivalCapacity = mRoutePoiDetailsPageView.poiArrivalCapacity;
            //50%以上电量，显示满电量图片，20-50%电量，显示半电量图片
            //0-20电量，显示低电量图片，文本变红
            //小于0%电量，显示空电量图片，文本变红
            if (leftCharge >= 50 && leftCharge <= 100) {
                sivArrivalCapacity.setImageResource(com.sgm.navi.scene.R.drawable.img_electricity_full_42);
                poiArrivalCapacity.setTextColor(
                        ResourceUtils.Companion.getInstance().getColor(com.sgm.navi.scene.R.color.text_color_route_item_select));
            } else if (leftCharge > 20 && leftCharge < 50) {
                sivArrivalCapacity.setImageResource(com.sgm.navi.scene.R.drawable.img_electricity_medium_42);
                poiArrivalCapacity.setTextColor(
                        ResourceUtils.Companion.getInstance().getColor(com.sgm.navi.scene.R.color.text_color_route_item_select));
            } else if (leftCharge > 0 && leftCharge <= 20) {
                sivArrivalCapacity.setImageResource(com.sgm.navi.scene.R.drawable.img_electricity_low_42);
                poiArrivalCapacity.setTextColor(
                        ResourceUtils.Companion.getInstance().getColor(com.sgm.navi.scene.R.color.search_color_delete_bg));
            } else if (leftCharge <= 0) {
                sivArrivalCapacity.setImageResource(com.sgm.navi.scene.R.drawable.img_electricity_empty_42);
                poiArrivalCapacity.setTextColor(
                        ResourceUtils.Companion.getInstance().getColor(com.sgm.navi.scene.R.color.search_color_delete_bg));
            }
        }
    }

    /***
     * 展示POI详情的加油站数据
     * @param info POI数据
     */
    public void showPOIDetailGas(final PoiInfoEntity info) {
        if (null == mRoutePoiDetailsPageView) {
            Logger.e(TAG, "mRoutePoiDetailsPageView = null");
            return;
        }
        mRoutePoiDetailsPageView.sivArrivalCapacity.setVisibility(View.GONE);
        mRoutePoiDetailsPageView.poiArrivalCapacity.setVisibility(View.GONE);
        final List<GasStationInfo> gasStationInfos = info.getStationList();
        for (GasStationInfo gasStationInfo : gasStationInfos) {
            gasStationInfo.setPrice(getContext().getString(R.string.route_oil_price, gasStationInfo.getPrice()));
        }
        final GasStationAdapter gasStationAdapter = new GasStationAdapter();
        gasStationAdapter.setGasStationList(gasStationInfos);
        ScenePoiDetailsGasStationViewBinding scenePoiDetailsGasStationView = mRoutePoiDetailsPageView.scenePoiDetailsGasStationView;
        scenePoiDetailsGasStationView.poiGasBusinessHours.
                setText(getContext().getString(R.string.route_business_hour, info.getBusinessTime()));
        if (ConvertUtils.isEmpty(info.getPhone())) {
            mRoutePoiDetailsPageView.scenePoiDetailsGasStationView.poiGasPhone.setVisibility(View.GONE);
        }
        scenePoiDetailsGasStationView.poiGasPhone.setText(
                getContext().getString(R.string.route_poi_phone, info.getPhone()));
        scenePoiDetailsGasStationView.poiGasOilList.setLayoutManager(
                new GridLayoutManager(getContext(), mSpanCount));
        scenePoiDetailsGasStationView.poiGasOilList.setAdapter(gasStationAdapter);
    }

    /***
     * 展示POI详情的按钮状态
     * @param isStartOrEnd 是否是起终点
     */
    public void showPOIButton(final boolean isStartOrEnd) {
        if (null == mRoutePoiDetailsPageView) {
            Logger.e(TAG, "mRoutePoiDetailsPageView = null");
            return;
        }
        if (isStartOrEnd) {
            mRoutePoiDetailsPageView.stvStartRoute.setAlpha(0.5f);
            mRoutePoiDetailsPageView.stvStartRoute.setClickable(false);
        } else {
            mRoutePoiDetailsPageView.stvStartRoute.setAlpha(1);
            mRoutePoiDetailsPageView.stvStartRoute.setClickable(true);
        }
    }
    //------------充电&POI详情页面***************************************************/


    @Override
    public void onInitData() {
        mViewModel.setDefaultPlateNumberAndAvoidLimitSave();
    }

    @Override
    public void onGetFragmentData() {
        final Bundle bundle = getArguments();
        assert bundle != null;
        final RouteSpeechRequestParam param = (RouteSpeechRequestParam) bundle.getSerializable("speech_open_route");
        if (!ConvertUtils.isEmpty(param)) {
            assert param != null;
            final PoiInfoEntity mEndPoiInfoEntity = param.getMEndPoiInfoEntity();
            if (!ConvertUtils.isEmpty(mEndPoiInfoEntity)) {
                mViewModel.getTitle().set(mEndPoiInfoEntity.getMName());
                mViewModel.getEndName().set(mEndPoiInfoEntity.getMName());
            }
            mViewModel.requestRouteFromSpeech(param);
            return;
        }

        final RouteMsgPushInfo routeMsgPushInfo = (RouteMsgPushInfo) bundle.getSerializable(
                AutoMapConstant.SearchBundleKey.BUNDLE_KEY_MSG_PUSH_OPEN_ROUTE_TYPE);
        if (!ConvertUtils.isEmpty(routeMsgPushInfo)) {
            mViewModel.getTitle().set(routeMsgPushInfo.getMName());
            mViewModel.getEndName().set(routeMsgPushInfo.getMName());
            mViewModel.requestRouteRestoration(routeMsgPushInfo);
            return;
        }

        final PoiInfoEntity poiInfoEntity = (PoiInfoEntity) bundle.getParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_ROUTE);
        final int poiType = (int) bundle.getInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_ROUTE_TYPE);
        if (!ConvertUtils.isEmpty(poiInfoEntity)) {
            mViewModel.getTitle().set(poiInfoEntity.getName());
            mViewModel.getEndName().set(poiInfoEntity.getName());
            final RouteRequestParam routeRequestParam = new RouteRequestParam();
            routeRequestParam.setMPoiInfoEntity(poiInfoEntity);
            routeRequestParam.setMRoutePoiType(poiType);
            mViewModel.requestRoute(routeRequestParam);

            //子poi显示
            if (poiInfoEntity.getMChildType() != AutoMapConstant.ChildType.DEFAULT
                    && poiInfoEntity.getMChildType() != AutoMapConstant.ChildType.CHILD_NO_GRAND) {
                mViewModel.setSecondaryPoiInfo(poiInfoEntity);
                setRouteSecondaryPoiUI(poiInfoEntity.getMChildType() , poiInfoEntity);
                mViewModel.setSecondaryPoi(true);
                mViewModel.showSecondaryPoi();
            }
            return;
        }
        closeFragment(true);
        ToastUtils.Companion.getInstance().showCustomToastView(
                ResourceUtils.Companion.getInstance().getString(R.string.route_error_no_request_data));
    }

    @Override
    public void onReStoreFragment() {
        if (!ConvertUtils.isEmpty(mViewModel)) {
            mViewModel.onReStoreFragment();
        }
    }

    @Override
    public void onHiddenChanged(final boolean hidden) {
        super.onHiddenChanged(hidden);
        if (!hidden) {
            //设置全览态
            ImmersiveStatusScene.getInstance().setImmersiveStatus(MapType.MAIN_SCREEN_MAIN_MAP, ImersiveStatus.IMERSIVE);
            if (!ConvertUtils.isEmpty(mViewModel)) {
                mViewModel.isRequestRouteForPlateNumberAndAvoidLimitChange();
            } else {
                Logger.e(TAG, ROUTE_ERROR);
            }
            if (!ConvertUtils.isEmpty(mBinding) && !ConvertUtils.isEmpty(mRouteListPageView.routeLineInfoSceneRoutePerference)) {
                mRouteListPageView.routeLineInfoSceneRoutePerference.resetPreference();
            } else {
                Logger.e(TAG, ROUTE_ERROR);
            }
        }
    }

    /***
     * 算路请求弹框展示
     */
    public void showProgressUI() {
        if (!ConvertUtils.isEmpty(mRouteRequestLoadingDialog) && mRouteRequestLoadingDialog.isShowing()) {
            Logger.d("mRouteRequestLoadingDialog is showing");
            return;
        }
        final Context context = this.getContext();
        if (context == null) {
            return;
        }
        if (isAdded() && getActivity() != null && !getActivity().isFinishing()) {
            mRouteRequestLoadingDialog = new RouteRequestLoadingDialog(context);
            mRouteRequestLoadingDialog.setOnCloseClickListener(mViewModel);
            if (!ConvertUtils.isEmpty(mRouteRequestLoadingDialog)) {
                mRouteRequestLoadingDialog.show();
            }
        }
    }

    /**
     * 显示离线算路文言
     */
    public void showOfflineProgressUI() {
        if (!ConvertUtils.isEmpty(mRouteRequestLoadingDialog) && mRouteRequestLoadingDialog.isShowing()) {
            mRouteRequestLoadingDialog.showOfflineRouting();
        }
    }

    /***
     * 算路请求弹框关闭
     */
    public void hideProgressUI() {
        if (!ConvertUtils.isEmpty(mRouteRequestLoadingDialog)) {
            mRouteRequestLoadingDialog.dismiss();
            mRouteRequestLoadingDialog = null;
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

    /***
     * 关闭Trip弹框
     */
    public void hideTrip() {
        if (!ConvertUtils.isEmpty(mMsgTopDialog)) {
            mMsgTopDialog.cancel();
            mMsgTopDialog = null;
        }
    }

    public void changePage(RoutePageLevel routePageLevel) {
        switch (routePageLevel) {
            case ROUTE_DETAILS_LIST:
                if (null == mRouteDetailsListPageView || null == mRouteDetailsListPageStub) {
                    mBinding.routeDetailInfoRoot.setOnInflateListener((viewStub, view) -> {
                        mRouteDetailsListPageView = DataBindingUtil.bind(view);
                        if (mRouteDetailsListPageView != null) {
                            mRouteDetailsListPageView.setVariable(BR.ViewModel, mViewModel);
                            mRouteDetailsListPageView.setViewModel(mViewModel);
                            mRouteDetailsListPageView.executePendingBindings();
                        }
                    });
                    mRouteDetailsListPageStub = mBinding.routeDetailInfoRoot.getViewStub();
                    mRouteDetailsListPageStub.inflate();
                    initRouteDetailsPage();
                }
                break;
            case ROUTE_SEARCH_SERVICE_LIST:
                if (null == mRouteServiceListPageView || null == mRouteServiceListPageStub) {
                    mBinding.routeServiceListInfoRoot.setOnInflateListener((viewStub, view) -> {
                        mRouteServiceListPageView = DataBindingUtil.bind(view);
                        if (mRouteServiceListPageView != null) {
                            mRouteServiceListPageView.setVariable(BR.ViewModel, mViewModel);
                            mRouteServiceListPageView.setViewModel(mViewModel);
                            mRouteServiceListPageView.executePendingBindings();
                        }
                    });
                    mRouteServiceListPageStub = mBinding.routeServiceListInfoRoot.getViewStub();
                    mRouteServiceListPageStub.inflate();
                    initServiceListPage();
                }
                break;
            case ROUTE_CHARGE_GAS_LIST:
                if (null == mRouteChargeGasListPageView || null == mRouteChargeGasListPageStub) {
                    mBinding.routeChargeListInfoRoot.setOnInflateListener((viewStub, view) -> {
                        mRouteChargeGasListPageView = DataBindingUtil.bind(view);
                        if (mRouteChargeGasListPageView != null) {
                            mRouteChargeGasListPageView.setVariable(BR.ViewModel, mViewModel);
                            mRouteChargeGasListPageView.setViewModel(mViewModel);
                            mRouteChargeGasListPageView.executePendingBindings();
                        }
                    });
                    mRouteChargeGasListPageStub = mBinding.routeChargeListInfoRoot.getViewStub();
                    mRouteChargeGasListPageStub.inflate();
                    initChargeGasListPage();
                }
                break;
            case ROUTE_WEATHER_LIST:
                if (null == mRouteWeatherPageView || null == mRouteWeatherPageStub) {
                    mBinding.routeWeatherDetails.setOnInflateListener((viewStub, view) -> {
                        mRouteWeatherPageView = DataBindingUtil.bind(view);
                        if (mRouteWeatherPageView != null) {
                            mRouteWeatherPageView.setVariable(BR.ViewModel, mViewModel);
                            mRouteWeatherPageView.setViewModel(mViewModel);
                            mRouteWeatherPageView.executePendingBindings();
                        }
                    });
                    mRouteWeatherPageStub = mBinding.routeWeatherDetails.getViewStub();
                    mRouteWeatherPageStub.inflate();
                    initWeatherDetailsPage();
                }
                break;
            case ROUTE_POI_DETAILS_LIST:
                if (null == mRoutePoiDetailsPageView || null == mRoutePoiDetailsPageStub) {
                    mBinding.routePoiDetails.setOnInflateListener((viewStub, view) -> {
                        mRoutePoiDetailsPageView = DataBindingUtil.bind(view);
                        if (mRoutePoiDetailsPageView != null) {
                            mRoutePoiDetailsPageView.setVariable(BR.ViewModel, mViewModel);
                            mRoutePoiDetailsPageView.setViewModel(mViewModel);
                            mRoutePoiDetailsPageView.executePendingBindings();
                        }
                    });
                    mRoutePoiDetailsPageStub = mBinding.routePoiDetails.getViewStub();
                    mRoutePoiDetailsPageStub.inflate();
                    initPoiDetailsPage();
                }
                break;
            default:
                if (null == mRouteListPageView || null == mRouteListPageStub) {
                    mBinding.routeLineInfoRoot.setOnInflateListener((viewStub, view) -> {
                        mRouteListPageView = DataBindingUtil.bind(view);
                        if (mRouteListPageView != null) {
                            mRouteListPageView.setVariable(BR.ViewModel, mViewModel);
                            mRouteListPageView.setViewModel(mViewModel);
                            mRouteListPageView.executePendingBindings();
                        }
                    });
                    mRouteListPageStub = mBinding.routeLineInfoRoot.getViewStub();
                    mRouteListPageStub.inflate();
                    initRouteResultPage();
                }
                break;
        }
    }

    @Override
    public void onDestroy() {
        mViewModel.cancelTimer();
        removeAllViewStub(mRouteListPageStub);
        removeAllViewStub(mRouteDetailsListPageStub);
        removeAllViewStub(mRouteServiceListPageStub);
        removeAllViewStub(mRouteChargeGasListPageStub);
        removeAllViewStub(mRouteWeatherPageStub);
        removeAllViewStub(mRoutePoiDetailsPageStub);
        mRouteListPageStub = null;
        mRouteDetailsListPageStub = null;
        mRouteServiceListPageStub = null;
        mRouteChargeGasListPageStub = null;
        mRouteWeatherPageStub = null;
        mRoutePoiDetailsPageStub = null;
        if (!ConvertUtils.isEmpty(mRouteRequestLoadingDialog)) {
            mRouteRequestLoadingDialog.dismiss();
            mRouteRequestLoadingDialog = null;
        }

        if (!ConvertUtils.isEmpty(mSearchLoadingDialog)) {
            mSearchLoadingDialog.dismiss();
            mSearchLoadingDialog = null;
        }
        super.onDestroy();
    }

    private void removeAllViewStub(ViewStub viewStub) {
        if (viewStub != null && viewStub.getParent() == null) {
            // ViewStub已被inflate，需要手动释放
            View inflatedView = viewStub.getInflatedId() != View.NO_ID ?
                    getActivity().findViewById(viewStub.getInflatedId()) : null;
            if (inflatedView != null) {
                ((ViewGroup) inflatedView.getParent()).removeView(inflatedView);
            }
        }
    }
}