
package com.fy.navi.scene.ui.poi;

import android.animation.ValueAnimator;
import android.app.Activity;
import android.content.ActivityNotFoundException;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.text.Spannable;
import android.text.SpannableString;
import android.text.TextUtils;
import android.text.style.LeadingMarginSpan;
import android.util.AttributeSet;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.animation.LinearInterpolator;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.constraintlayout.widget.ConstraintSet;
import androidx.core.content.ContextCompat;
import androidx.fragment.app.Fragment;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.alibaba.android.arouter.launcher.ARouter;
import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.burypoint.anno.HookMethod;
import com.fy.navi.burypoint.bean.BuryProperty;
import com.fy.navi.burypoint.constant.BuryConstant;
import com.fy.navi.burypoint.controller.BuryPointController;
import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.R;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.scene.adapter.GasStationAdapter;
import com.fy.navi.scene.adapter.GridSpacingItemDecoration;
import com.fy.navi.scene.databinding.ScenePoiDetailsContentViewBinding;
import com.fy.navi.scene.impl.poi.ScenePoiDetailContentViewImpl;
import com.fy.navi.scene.impl.search.SearchFragmentFactory;
import com.fy.navi.scene.ui.adapter.PoiDetailsScenicChildAdapter;
import com.fy.navi.scene.ui.adapter.RoutePOIGasStationAdapter;
import com.fy.navi.scene.ui.route.SceneRouteDescendantsView;
import com.fy.navi.scene.ui.search.SearchConfirmDialog;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.layer.refix.LayerPointItemType;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.route.RoutePoiType;
import com.fy.navi.service.define.search.ChargeInfo;
import com.fy.navi.service.define.search.ChildInfo;
import com.fy.navi.service.define.search.ETAInfo;
import com.fy.navi.service.define.search.FavoriteInfo;
import com.fy.navi.service.define.search.GasStationInfo;
import com.fy.navi.service.define.search.ParkingInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.setting.SettingUpdateObservable;
import com.fy.navi.ui.action.ViewAdapterKt;
import com.fy.navi.ui.base.BaseFragment;
import com.fy.navi.ui.dialog.IBaseDialogClickListener;
import com.fy.navi.ui.view.SkinImageView;
import com.fy.navi.ui.view.SkinTextView;

import org.json.JSONException;
import org.json.JSONObject;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

/**
 * @author baipeng0904
 * @Description: 类作用描述
 * @CreateDate: $ $
 * @version \$Revision1.0\$
 */
public class ScenePoiDetailContentView extends BaseSceneView<ScenePoiDetailsContentViewBinding,
        ScenePoiDetailContentViewImpl> {
    private static final String HOME_COMPANY_FRAGMENT = "HomeCompanyFragment";
    private static final String DEFATULE_STRING = "--";
    private static final String DIVIDER = "_";
    private PoiInfoEntity mPoiInfoEntity;
    private final int mSpacing = 24; // 上下间距
    private final int mHorizontalSpacing = 32; // 左右间距
    private final int mChildSpacing = 24;//子POI info item间距
    private final int mSpanCount = 2;//数据列数
    private PoiInfoEntity mChildSelectInfo;
    private PoiInfoEntity mGrandChildSelectInfo;
    private int mPoiType;
    private boolean mViaAddType = true;
    private PoiDetailsScenicChildAdapter mScenicChildAdapter;
    private SearchResultEntity mSearchResultEntity;
    private boolean mIsOpenFromNavi;
    private int mChildSelectIndex = -1;
    private boolean mIsCollectStatus = false;
    private boolean mIsEnd = false;
    private ETAInfo mEtaInfo;
    private ValueAnimator mAnimator;
    private float mAngelTemp = 0;

    public ScenePoiDetailContentView(final @NonNull Context context) {
        super(context);
    }

    public ScenePoiDetailContentView(final @NonNull Context context,
                                     final @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public ScenePoiDetailContentView(final @NonNull Context context,
                                     final @Nullable AttributeSet attrs,
                                     final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected ScenePoiDetailsContentViewBinding createViewBinding(final LayoutInflater inflater,
                                                                  final ViewGroup viewGroup) {
        return ScenePoiDetailsContentViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected ScenePoiDetailContentViewImpl initSceneImpl() {
        return new ScenePoiDetailContentViewImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setScenePoiDetailContentView(mScreenViewModel);
    }

    @Override
    protected void initObserver() {

        mViewBinding.scenePoiDetailsBottomView.stlStartRoute.setOnClickListener(v ->
                handleRouteClick());
        mViewBinding.scenePoiDetailsBottomView.stlAroundSearch.setOnClickListener(v ->
                handleAroundSearchClick());
        mViewBinding.scenePoiDetailsBottomView.stlPoiFavorites.setOnClickListener(v ->{
                if(!ConvertUtils.isEmpty(mPoiInfoEntity.getOperatorId())){
                    if(!mScreenViewModel.isSGMLogin()){
                        mScreenViewModel.startSGMLogin();
                    }else{
                        handleNetFavoriteClick();
                    }
                }else{
                    handleFavoriteClick();
                }

            }
        );
        initLoadAnim(mViewBinding.ivLoading);
        updateRouteButton();
    }

    /**
     * 去这里按钮的点击事件
     */
    private void handleRouteClick() {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "点击去这里");
        final PoiInfoEntity poiInfo;
        if (mGrandChildSelectInfo != null) {
            poiInfo = mGrandChildSelectInfo;
        } else if (mChildSelectInfo != null) {
            poiInfo = mChildSelectInfo;
        } else {
            poiInfo = mPoiInfoEntity;
        }
        if (SearchPackage.getInstance().isAlongWaySearch() && !mIsEnd) {
            if (mViaAddType) {
                if(RoutePackage.getInstance().isMaxRouteParam(MapType.MAIN_SCREEN_MAIN_MAP)){
                    Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "handleRouteClick isMaxRouteParam");
                    return;
                }
                RoutePackage.getInstance().addViaPoint(MapType.MAIN_SCREEN_MAIN_MAP,
                        poiInfo);
            } else {
                RoutePackage.getInstance().removeVia(MapType.MAIN_SCREEN_MAIN_MAP,
                        poiInfo, true);
            }

        } else {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "end point1: " + poiInfo.getPoint().getLon()
                    + " ,lat" + poiInfo.getPoint().getLat());
            if (mIsEnd) {
                RoutePackage.getInstance().requestChangeEnd(mMapTypeId, poiInfo);
            } else {
                openRouteFragment(poiInfo);
            }
        }

    }

    /**
     * 打开路线界面
     * @param poiInfoEntity poi信息
     */
    @HookMethod(eventName = BuryConstant.EventName.AMAP_DESTINATION_GO)
    private void openRouteFragment(final PoiInfoEntity poiInfoEntity) {
        final Fragment fragment = (Fragment) ARouter.getInstance().build(
                RoutePath.Route.ROUTE_FRAGMENT).navigation();
        addFragment((BaseFragment) fragment, SearchFragmentFactory.createRouteFragment(poiInfoEntity));

        //for burying point
        final JSONObject params = new JSONObject();
        try {
            params.put(BuryConstant.Key.ROUTE_POI_TYPE, RoutePoiType.ROUTE_POI_TYPE_END);
            params.put(BuryConstant.Key.POI_INFO_ENTRY, poiInfoEntity);
        } catch (JSONException e) {
            e.printStackTrace();
        }
        final BuryProperty properties = new BuryProperty.Builder()
                .setParams(BuryConstant.ProperType.BURY_KEY_SEARCH_CONTENTS, params.toString())
                .build();
        BuryPointController.getInstance().setBuryProps(properties);
    }

    /**
     * 周边搜索按钮的点击事件
     */
    @HookMethod(eventName = BuryConstant.EventName.AMAP_DESTINATION_NEARBY)
    private void handleAroundSearchClick() {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "点击周边搜索");
        final Fragment fragment = (Fragment) ARouter.getInstance().build(
                RoutePath.Search.AROUND_SEARCH_FRAGMENT).navigation();
        addFragment((BaseFragment) fragment,
                SearchFragmentFactory.createAroundFragment(mPoiInfoEntity));

        //for burying point
        final BuryProperty properties = new BuryProperty.Builder()
                .setParams(BuryConstant.ProperType.BURY_KEY_SEARCH_CONTENTS, mPoiInfoEntity.getName())
                .build();
        BuryPointController.getInstance().setBuryProps(properties);
    }

    @HookMethod
    private void sendBuryPointForAddFavorite(final String name, final int type) {
        String eventName = "";
        String key = BuryConstant.ProperType.BURY_KEY_HOME_PREDICTION;
        switch (type) {
            case 0:
                eventName = BuryConstant.EventName.AMAP_SETTING_FAVORITE_ADD;
                key = BuryConstant.ProperType.BURY_KEY_SEARCH_CONTENTS;
                break;
            case 1:
                eventName = BuryConstant.EventName.AMAP_HOME_SAVE;
                break;
            case 2:
                eventName = BuryConstant.EventName.AMAP_WORK_SAVE;
                break;
            case 3:
                eventName = BuryConstant.EventName.AMAP_SETTING_HOT_ADD;
                key = BuryConstant.ProperType.BURY_KEY_SEARCH_CONTENTS;
                break;
        }
        BuryPointController.getInstance().setEventName(eventName);
        BuryProperty buryProperty = new BuryProperty.Builder()
                .setParams(key, name)
                .build();
        BuryPointController.getInstance().setBuryProps(buryProperty);
    }

    @HookMethod(eventName = BuryConstant.EventName.AMAP_FAVORITE_SAVE)
    private void sendBuryPointForAddFavoriteFromMap(final String name) {
        BuryProperty buryProperty = new BuryProperty.Builder()
                .setParams(BuryConstant.ProperType.BURY_KEY_HOME_PREDICTION, name)
                .build();
        BuryPointController.getInstance().setBuryProps(buryProperty);
    }

    /**
     * 收藏按钮的点击事件
     */
    private void handleFavoriteClick() {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "点击收藏");
        if (mPoiInfoEntity != null) {
            SettingUpdateObservable.getInstance().onUpdateSyncTime();

            final boolean isFavorite = !mScreenViewModel.isFavorite(mPoiInfoEntity).isEmpty();

            final int favoriteIcon = isFavorite ? R.drawable.img_star58 :
                    R.drawable.img_star_filling58;
            mViewBinding.scenePoiDetailsBottomView.sivPoiFavorites.setImageDrawable(
                    ContextCompat.getDrawable(getContext(), favoriteIcon));
            if (mPoiType == AutoMapConstant.PoiType.POI_MAP_CAR_CLICK) {
                final int favIcon = isFavorite ? R.drawable.img_star_white58 :
                        R.drawable.img_star_filling58;
                mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setImageDrawable(
                        ContextCompat.getDrawable(getContext(), favIcon));
                mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setText(isFavorite ? R.string.sha_favorite : R.string.sha_has_favorite);
            }

            if (isFavorite) {
                mScreenViewModel.removeFavorite(mPoiInfoEntity);
//                mScreenViewModel.deleteFavoriteData(mPoiInfoEntity.getFavoriteInfo().getItemId());
                ToastUtils.Companion.getInstance().showCustomToastView("取消收藏");
            } else {
                if (ConvertUtils.isEmpty(mPoiInfoEntity.getPid())) {
                    //逆地理搜索出的点无poiId，需自己拼接
                    mPoiInfoEntity.setPid(mPoiInfoEntity.getPoint().getLon() + "_"
                            + mPoiInfoEntity.getPoint().getLat());
                }
                mScreenViewModel.addFavorite(mPoiInfoEntity, 0);
                sendBuryPointForAddFavoriteFromMap(mPoiInfoEntity.getName());
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "current : "
                        + mPoiInfoEntity.getPoint().getLon() + " " + mPoiInfoEntity.getPoint().getLat()
                        + " ID: " + mPoiInfoEntity.getPid() + " ,name: " + mPoiInfoEntity.getName());
//                mScreenViewModel.addFavoriteData(mPoiInfoEntity, 0);
                ToastUtils.Companion.getInstance().showCustomToastView("收藏成功");
            }
        }
    }

    private void handleNetFavoriteClick(){
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "点击自营站收藏");
        mScreenViewModel.updateCollectStatus((Activity) getContext(),mPoiInfoEntity);
    }

    /**
     * 添加途径点按钮点击事件
     */
    private void updateRouteButton() {
        final boolean isAlongWaySearch = mScreenViewModel.isAlongWaySearch() && !mIsEnd;
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, isAlongWaySearch ? "添加途径点" : "去这里");

        final int routeIcon = isAlongWaySearch ? R.drawable.img_basic_ic_add :
                R.drawable.icon_details_bottom_go_here;
        final int routeText = isAlongWaySearch ? R.string.st_along_way_point : R.string.st_go_here;

        mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setImageDrawable(
                ContextCompat.getDrawable(getContext(), routeIcon));
        mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setText(routeText);
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
     * 搜索结果回调
     * @param taskId 请求Id
     * @param searchResultEntity 数据实体类
     * @noinspection checkstyle:LeftCurly
     */
    public void onSearchResult(final int taskId, final SearchResultEntity searchResultEntity) {
        if (null == searchResultEntity || searchResultEntity.getPoiList().isEmpty() || ConvertUtils.isEmpty(mScreenViewModel)) {
            //ToastUtils.Companion.getInstance().showCustomToastView("暂无数据");
            return;
        }
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "taskId: " + taskId
                + " currentId: " + mScreenViewModel.getMTaskId());
        if (!ConvertUtils.equals(taskId, mScreenViewModel.getMTaskId()) && mScreenViewModel.getMTaskId() != 0) {
            return;
        }
        showLoading(false);
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "poiType: " + searchResultEntity.getPoiType());
        if (searchResultEntity.getPoiType() == 0) {
            final CityDataInfo cityDataInfo;
            if(!ConvertUtils.isEmpty(searchResultEntity.getPoiList())) {
                final PoiInfoEntity poiInfoEntity = searchResultEntity.getPoiList().get(0);
                cityDataInfo = mScreenViewModel.getCityInfo(poiInfoEntity.getAdCode());
            } else {
                cityDataInfo = mScreenViewModel.getCityInfo(mScreenViewModel.getAcCode());
            }
            if (cityDataInfo != null) {
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "城市数据信息: " + cityDataInfo.getName() +"，城市编码: "
                        + mScreenViewModel.getAcCode());
                mViewBinding.poiOfflineHint.setVisibility(VISIBLE);
                mViewBinding.poiOfflineHint.setText(
                        getContext().getString(R.string.search_offline_hint, cityDataInfo.getName()));
            }
            mViewBinding.poiDetailsScroll.setMaxHeight(316);
            mViewBinding.sivArrivalCapacity.setVisibility(View.GONE);
            mViewBinding.poiDistanceTime.setVisibility(View.GONE);
            mViewBinding.poiArrivalCapacity.setVisibility(View.GONE);

        }
        mSearchResultEntity = searchResultEntity;
        this.mPoiInfoEntity = searchResultEntity.getPoiList().get(0);
        initNormalView();
        if (mPoiInfoEntity != null && mScreenViewModel != null) {
            ThreadManager.getInstance().removeHandleTask(mTimeoutTask);
            final int pointTypeCode = mScreenViewModel.getPointTypeCode(mPoiInfoEntity.getPointTypeCode());
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "pointTypeCode is: " + pointTypeCode);
            switch (pointTypeCode) {
                case AutoMapConstant.PointTypeCode.GAS_STATION:
                    refreshGasStationView();
                    break;
                case AutoMapConstant.PointTypeCode.CHARGING_STATION:
                    refreshChargeStationView();
                    break;
                case AutoMapConstant.PointTypeCode.CAR_WASH:
                    refreshCarWashView();
                    break;
                case AutoMapConstant.PointTypeCode.CATERING:
                    refreshCateringView();
                    break;
                case AutoMapConstant.PointTypeCode.PARKING_LOT:
                    refreshParkingLotView();
                    break;
                case AutoMapConstant.PointTypeCode.SERVICE_AREA:
                    refreshServiceAreaView();
                    break;
                case AutoMapConstant.PointTypeCode.SCENIC_SPOT:
                    refreshScenicSpotView();
                    break;
                case AutoMapConstant.PointTypeCode.OTHERS:
                default:
                    refreshNormalView();
                    break;
            }
        }
        if (mPoiType == AutoMapConstant.PoiType.POI_MAP_CAR_CLICK && mViewBinding != null) {
            mViewBinding.skPoiName.setText(R.string.shc_my_point);
            mViewBinding.poiDistanceTime.setVisibility(View.GONE);
            mViewBinding.poiArrivalCapacity.setVisibility(View.GONE);
            mViewBinding.sivArrivalCapacity.setVisibility(View.GONE);
            mViewBinding.poiContentLayout.setVisibility(View.GONE);
            mViewBinding.poiDetailsSubLine.setVisibility(View.GONE);
        }
        if (mPoiType == AutoMapConstant.PoiType.POI_MAP_CLICK) {
            //点击地图弹出的poi详情页不需要展示子点扎标
            if (null != mScreenViewModel) {
                ThreadManager.getInstance().postDelay(() -> {
                    if (mScreenViewModel != null) {
                        mScreenViewModel.clearTypeMark(LayerPointItemType.SEARCH_CHILD_POINT);
                    }
                }, 400);
            }
        }
    }

    public void onSilentSearchResult(int taskId,SearchResultEntity mSearchResultEntity){
        if (null == mSearchResultEntity || mSearchResultEntity.getPoiList().isEmpty() || ConvertUtils.isEmpty(mScreenViewModel)) {
            //ToastUtils.Companion.getInstance().showCustomToastView("暂无数据");
            return;
        }
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "taskId: " + taskId
                + " currentId: " + mScreenViewModel.getMTaskId());

        if (!ConvertUtils.equals(taskId, mScreenViewModel.getMTaskId()) && mScreenViewModel.getMTaskId() != 0) {
            return;
        }
        ArrayList<PoiInfoEntity> list = new ArrayList<>();
        list.add(mSearchResultEntity.getPoiList().get(0));
        SearchResultEntity searchResultEntity = new SearchResultEntity()
                .setPoiList(list)
                .setPoiType(1);
        onSearchResult(mScreenViewModel.getMTaskId(),searchResultEntity);
        reloadPoiLabelMarker();
    }

    public void onCollectUpdate(String code){
        if("0000".equals(code)){
            mIsCollectStatus = !mIsCollectStatus;
            final int favoriteIcon = !mIsCollectStatus ? R.drawable.img_star58 :
                    R.drawable.icon_basic_ic_star_fav;
            mViewBinding.scenePoiDetailsBottomView.sivPoiFavorites.setImageDrawable(
                    ContextCompat.getDrawable(getContext(), favoriteIcon));
            ToastUtils.Companion.getInstance().showCustomToastView(mIsCollectStatus ? getContext().getString(R.string.sha_has_favorite) : getContext().getString(R.string.sha_cancel_favorite));
        }
    }

    /**
     * 搜索图层子点点击事件
     * @param index 点击下标
     */
    public void onMarkChildClickCallBack(final int index) {
        if (mPoiInfoEntity == null || ConvertUtils.isEmpty(mPoiInfoEntity.getChildInfoList())) {
            return;
        }
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onMarkChildClickCallBack is: " + index);
        final List<ChildInfo> mChildList = mPoiInfoEntity.getChildInfoList();
        if (index < mChildList.size() && index >= 0) {
            final ChildInfo childInfo = mChildList.get(index);
            mChildSelectInfo = new PoiInfoEntity()
                    .setName(childInfo.getName())
                    .setAddress(childInfo.getAddress())
                    .setPid(childInfo.getPoiId())
                    .setChildInfoList(childInfo.getMGrandChildInfoList())
                    .setPoint(childInfo.getLocation());
            if (ConvertUtils.isEmpty(childInfo.getMGrandChildInfoList())) {
                mChildSelectInfo.setMChildType(AutoMapConstant.ChildType.CHILD_NO_GRAND);
                mViewBinding.lySecondaryPoi.setVisibility(View.GONE);
            } else {
                mChildSelectInfo.setMChildType(AutoMapConstant.ChildType.CHILD_HAS_GRAND);
                mViewBinding.lySecondaryPoi.setVisibility(View.VISIBLE);
                mViewBinding.lySecondaryPoi.setUIMode(AutoMapConstant.ChildType.CHILD_HAS_GRAND, mChildSelectInfo);
                mViewBinding.lySecondaryPoi.setItemClickListener(new SceneRouteDescendantsView.OnItemClickListener() {
                    @Override
                    public void onItemClick(final PoiInfoEntity poiInfo) {
                        mGrandChildSelectInfo = poiInfo;
                    }

                    @Override
                    public void onCancelSelectClick(final PoiInfoEntity poiInfoEntity) {
                        mGrandChildSelectInfo = null;
                    }

                    @Override
                    public void OnScrollListener() {
                    }
                });
            }
            mGrandChildSelectInfo = null;
            for (int i = 0; i < mChildList.size(); i++) {
                if (i == index) {
                    mChildList.get(i).setChecked(1);
                } else {
                    mChildList.get(i).setChecked(-1);
                }
            }
            if (mScenicChildAdapter != null) {
                mScenicChildAdapter.setChildInfoList(mChildList);
            }
        }
    }

    /**
     * 刷新ETA信息 距离，到达时间，预计剩余电量
     */
    private void refreshEtaInfoView() {
        mScreenViewModel.getTravelTimeFuture(new GeoPoint(mPoiInfoEntity.getPoint().getLon(),
                        mPoiInfoEntity.getPoint().getLat()))
                .thenAccept(etaInfo -> {
                    ThreadManager.getInstance().postUi(new Runnable() {
                        @Override
                        public void run() {
                            if (!ConvertUtils.isEmpty(etaInfo) && !ConvertUtils.isEmpty(mViewBinding)
                                    && !ConvertUtils.isEmpty(mViewBinding.poiDistanceTime)) {
                                final String distance = formatDistanceArrayInternal(
                                        etaInfo.getDistance());
                                mEtaInfo = etaInfo;
                                mViewBinding.poiDistanceTime.setText(MessageFormat.format("{0} {1}",
                                        distance, etaInfo.getTravelTime()));
                                final int leftCharge = Math.max(-99, etaInfo.getLeftCharge());
                                if (!ConvertUtils.isEmpty(leftCharge)) {
                                    //50%以上电量，显示满电量图片，20-50%电量，显示半电量图片
                                    //0-20电量，显示低电量图片，文本变红
                                    //小于0%电量，显示空电量图片，文本变红
                                    if (leftCharge >= 50 && leftCharge <= 100) {
                                        mViewBinding.sivArrivalCapacity.setImageResource(R.drawable.img_electricity_full_42);
                                        mViewBinding.poiArrivalCapacity.setTextColor(
                                                ResourceUtils.Companion.getInstance().getColor(R.color.poi_details_bottom_ff_00));
                                    } else if (leftCharge > 20 && leftCharge < 50) {
                                        mViewBinding.sivArrivalCapacity.setImageResource(R.drawable.img_electricity_medium_42);
                                        mViewBinding.poiArrivalCapacity.setTextColor(
                                                ResourceUtils.Companion.getInstance().getColor(R.color.poi_details_bottom_ff_00));
                                    } else if (leftCharge > 0 && leftCharge <= 20) {
                                        mViewBinding.sivArrivalCapacity.setImageResource(R.drawable.img_electricity_low_42);
                                        mViewBinding.poiArrivalCapacity.setTextColor(
                                                ResourceUtils.Companion.getInstance().getColor(R.color.navi_color_C73333_100));
                                    } else if (leftCharge <= 0) {
                                        mViewBinding.sivArrivalCapacity.setImageResource(R.drawable.img_electricity_empty_42);
                                        mViewBinding.poiArrivalCapacity.setTextColor(
                                                ResourceUtils.Companion.getInstance().getColor(R.color.navi_color_C73333_100));
                                    }
                                }
                                mViewBinding.poiArrivalCapacity.setText(getContext().getString(
                                        R.string.remain_charge, leftCharge));
                            }
                        }
                    });

                })
                .exceptionally(error -> {
                    Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "getTravelTimeFuture error:" + error);
                    return null;
                });
    }

    private boolean mIsPhoneExpanded = false;

    /**
     * 初始化电话下拉按钮
     * @param phoneIconView 下拉按钮
     * @param phone 电话
     * @param hasMultiple 是否有多个电话
     * @param phones 电话数组
     */
    private void initPoiPhoneIconObserver(final SkinImageView phoneIconView, final SkinTextView phone,
                                          final boolean hasMultiple, final String[] phones) {

        // 添加下拉按钮逻辑
        phoneIconView.setVisibility(hasMultiple ? View.VISIBLE : View.GONE);
        phoneIconView.setOnClickListener(v -> {
            mIsPhoneExpanded = !mIsPhoneExpanded;
            final String newText = getContext().getString(R.string.poi_phone,
                    mIsPhoneExpanded ? mPoiInfoEntity.getPhone() : phones[0]).replace(";","\n");

            // 计算前缀文字宽度
            final String prefix = getContext().getString(R.string.poi_phone, "").replace("%s", "");
            final float prefixWidth = phone.getPaint().measureText(prefix);
            final SpannableString spannable = new SpannableString(newText);
            final int newLinePos = newText.indexOf("\n");
            if (newLinePos >= 0) {
                spannable.setSpan(
                        new LeadingMarginSpan.Standard((int) prefixWidth, 0),
                        newLinePos + 1,
                        newText.length(),
                        Spannable.SPAN_EXCLUSIVE_EXCLUSIVE
                );
            }

            phone.setText(spannable);
            phoneIconView.setImageResource(
                    mIsPhoneExpanded ? R.drawable.img_up_48 : R.drawable.img_under_the_48);
        });
    }

    private boolean mIsBusinessTimeExpanded = false;


    /**
     * 初始化营业时间下拉按钮
     * @param businessTimeIconView 下拉按钮
     * @param businessTime 营业时间
     * @param hasMultiple 是否有多个营业时间
     * @param businessTimes 营业时间数组
     */
    private void initPoiBusinessTimeIconObserver(final SkinImageView businessTimeIconView, final SkinTextView businessTime,
                                          final boolean hasMultiple, final String[] businessTimes) {

        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "initPoiBusinessTimeIconObserver hasMultiple: " + hasMultiple);

        // 添加下拉按钮逻辑
        businessTimeIconView.setVisibility(hasMultiple ? View.VISIBLE : View.GONE);
        businessTimeIconView.setOnClickListener(v -> {
            mIsBusinessTimeExpanded = !mIsBusinessTimeExpanded;
            final String newText = getContext().getString(R.string.business_hour,
                    mIsBusinessTimeExpanded ? mPoiInfoEntity.getBusinessTime() : businessTimes[0]).replace("；","\n");

            businessTime.setText(newText);
            businessTimeIconView.setImageResource(
                    mIsBusinessTimeExpanded ? R.drawable.img_up_48 : R.drawable.img_under_the_48);
        });
    }

    /**
     * 刷新通用视图
     */
    private void initNormalView() {
        if (mViewBinding != null && mPoiInfoEntity != null) {
            mViewBinding.skPoiName.setText(mPoiInfoEntity.getName());
            mViewBinding.poiSecondAddress.setText(mPoiInfoEntity.getAddress());
            if (ConvertUtils.isEmpty(mPoiInfoEntity.getBusinessTime())) {
                mViewBinding.scenePoiDetailsNormalView.poiBusinessHoursLayout.setVisibility(View.GONE);
            } else {
                final String[] businessTimes = mPoiInfoEntity.getBusinessTime().split("；");
                final String text = getContext().getString(R.string.business_hour, businessTimes[0]);

                initPoiBusinessTimeIconObserver(mViewBinding.scenePoiDetailsNormalView.poiBusinessIcon,
                        mViewBinding.scenePoiDetailsNormalView.poiBusinessHours, businessTimes.length > 1, businessTimes);
                mViewBinding.scenePoiDetailsNormalView.poiBusinessHours.setText(text);
            }
            if (ConvertUtils.isEmpty(mPoiInfoEntity.getPhone())) {
                mViewBinding.scenePoiDetailsNormalView.poiPhone.setVisibility(View.GONE);
                mViewBinding.scenePoiDetailsBottomView.stlPhone.setVisibility(View.GONE);
            }else{
                final String[] phones = mPoiInfoEntity.getPhone().split(";");
                final String text = getContext().getString(R.string.poi_phone, phones[0]);
                initPoiPhoneIconObserver(mViewBinding.scenePoiDetailsNormalView.poiPhoneIcon,
                        mViewBinding.scenePoiDetailsNormalView.poiPhone,
                        phones.length > 1, phones);
                mViewBinding.scenePoiDetailsNormalView.poiPhone.setText(text);
            }
            mViewBinding.scenePoiDetailsBottomView.stlPhone.setOnClickListener(new OnClickListener() {
                @Override
                @HookMethod(eventName = BuryConstant.EventName.AMAP_DESTINATION_PHONE)
                public void onClick(final View v) {
                    final String phone = mPoiInfoEntity.getPhone();
                    final List<String> phoneString = new ArrayList<>();
                    final StringBuffer phoneProp = new StringBuffer();
                    if (phone.contains(";")) {
                        final String[] split = phone.split(";");
                        phoneString.addAll(Arrays.asList(split));
                    } else {
                        phoneString.add(phone);
                    }
                    if (!ConvertUtils.isEmpty(phoneString) && !ConvertUtils.isEmpty(phoneString.get(0))) {
                        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "call phone: " + phoneString.get(0));
                        phoneProp.append(phoneString.get(0));
                        new SearchConfirmDialog.Build(getContext())
                                .setDialogObserver(new IBaseDialogClickListener() {
                                    @Override
                                    public void onCommitClick() {
                                        try {
                                            //拨打电话
                                            final Intent intent = new Intent();
                                            intent.setAction(Intent.ACTION_CALL);
                                            intent.setData(Uri.parse("tel:" + phoneString.get(0)));
                                            final Context context = getContext();
                                            context.startActivity(intent);
                                        } catch (ActivityNotFoundException e) {// 提示用户无法拨打电话
                                            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "No app found to handle the call action: " + e.getMessage());
                                        } catch (SecurityException e) {// 提示用户权限不足
                                            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "Permission denied for making a call: " + e.getMessage());
                                        } catch (NullPointerException e) {// 提示用户数据无效
                                            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "Null pointer exception: " + e.getMessage());
                                        } catch (IllegalArgumentException e) {// 提示用户电话号码无效
                                            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "Invalid phone number: " + e.getMessage());
                                        }
                                    }
                                    @Override
                                    public void onCancelClick() {
                                    }
                                })
                                .setContent(getContext().getString(R.string.text_dial_phone_content, phoneString.get(0)))
                                .setConfirmTitle(getContext().getString(R.string.text_dial))
                                .build().show();

                    } else {
                        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "call phone is null ");
                        phoneProp.append("");
                    }

                    //For burying point
                    final BuryProperty buryProperty = new BuryProperty.Builder()
                            .setParams(BuryConstant.ProperType.BURY_KEY_SEARCH_CONTENTS, phoneProp.toString())
                            .build();
                    BuryPointController.getInstance().setBuryProps(buryProperty);

                }
            });
            final FavoriteInfo favoriteInfo = new FavoriteInfo()
                    .setCommonName(0)
                    .setUpdateTime(new Date().getTime());
            if (ConvertUtils.isEmpty(mPoiInfoEntity.getPid())) {
                //逆地理搜索出的点无poiId，需自己拼接
                mPoiInfoEntity.setPid(mPoiInfoEntity.getPoint().getLon() + "_"
                        + mPoiInfoEntity.getPoint().getLat());
            }
            final String itemId = mScreenViewModel.isFavorite(mPoiInfoEntity);
            mPoiInfoEntity.setFavoriteInfo(favoriteInfo);
            if (!itemId.isEmpty() || mPoiInfoEntity.getIsCollect()) {
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"IsCollect");
                mIsCollectStatus = true;
                mPoiInfoEntity.getFavoriteInfo().setItemId(itemId);
                mViewBinding.scenePoiDetailsBottomView.sivPoiFavorites.setImageDrawable(
                        ContextCompat.getDrawable(getContext(), R.drawable.icon_basic_ic_star_fav));
                if (mPoiType == AutoMapConstant.PoiType.POI_MAP_CAR_CLICK) {
                    mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setImageDrawable(
                            ContextCompat.getDrawable(getContext(), R.drawable.icon_basic_ic_star_fav));
                    mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setText(R.string.sha_has_favorite);
                }
            } else {
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"IsNoCollect");
                mIsCollectStatus = false;
                mViewBinding.scenePoiDetailsBottomView.sivPoiFavorites.setImageDrawable(
                        ContextCompat.getDrawable(getContext(),
                                R.drawable.img_star58));
                if (mPoiType == AutoMapConstant.PoiType.POI_MAP_CAR_CLICK) {
                    mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setImageDrawable(
                            ContextCompat.getDrawable(getContext(), R.drawable.img_star_white58));
                    mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setText(R.string.sha_favorite);
                }
            }
            refreshEtaInfoView();
        } else {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "mViewBinding or poiInfoEntity is null");
        }
    }

    /**
     * 刷新加油站视图
     */
    private void refreshGasStationView() {
        final List<GasStationInfo> gasStationInfos = mPoiInfoEntity.getStationList();
        for (GasStationInfo gasStationInfo : gasStationInfos) {
            gasStationInfo.setPrice(getContext().getString(R.string.oil_price, gasStationInfo.getPrice()));
        }
        final GasStationAdapter gasStationAdapter = new GasStationAdapter();
        gasStationAdapter.setGasStationList(gasStationInfos);
        if (ConvertUtils.isEmpty(mPoiInfoEntity.getBusinessTime())) {
            mViewBinding.scenePoiDetailsGasStationView.poiGasOilLayout.setVisibility(View.GONE);
        } else {
            final String[] businessTimes = mPoiInfoEntity.getBusinessTime().split("；");
            final String text = getContext().getString(R.string.business_hour, businessTimes[0]);

            initPoiBusinessTimeIconObserver(mViewBinding.scenePoiDetailsGasStationView.poiBusinessIcon,
                    mViewBinding.scenePoiDetailsGasStationView.poiGasBusinessHours, businessTimes.length > 1, businessTimes);
            mViewBinding.scenePoiDetailsGasStationView.poiGasBusinessHours.setText(text);
        }

        if (ConvertUtils.isEmpty(mPoiInfoEntity.getPhone())) {
            mViewBinding.scenePoiDetailsGasStationView.poiGasPhone.setVisibility(View.GONE);
        }

        final String[] phones = mPoiInfoEntity.getPhone().split(";");
        final String text = getContext().getString(R.string.poi_phone, phones[0]);
        initPoiPhoneIconObserver(mViewBinding.scenePoiDetailsGasStationView.poiPhoneIcon,
                mViewBinding.scenePoiDetailsGasStationView.poiGasPhone,
                phones.length > 1, phones);
        mViewBinding.scenePoiDetailsGasStationView.poiGasPhone.setText(text);
        mViewBinding.scenePoiDetailsGasStationView.poiGasOilList.setLayoutManager(
                new GridLayoutManager(getContext(), mSpanCount));
        mViewBinding.scenePoiDetailsGasStationView.poiGasOilList.addItemDecoration(
                new GridSpacingItemDecoration(getContext(), mSpanCount, mSpacing, mHorizontalSpacing,
                        false));
        mViewBinding.scenePoiDetailsGasStationView.poiGasOilList.setAdapter(gasStationAdapter);
        mViewBinding.scenePoiDetailsGasStationView.poiGasRoot.setVisibility(VISIBLE);
        mViewBinding.scenePoiDetailsChargingStationView.poiChargeRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsWashCarView.poiWashCarRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsCateringView.poiCateringRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsServiceAreaView.poiServiceAreaRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsNormalView.poiNormalRoot.setVisibility(GONE);
    }

    /**
     * 刷新充电桩视图
     */
    private void refreshChargeStationView() {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "imageUrl is: " + mPoiInfoEntity.getImageUrl());
        final List<ChargeInfo> chargeInfos = mPoiInfoEntity.getChargeInfoList();
        if (!ConvertUtils.isEmpty(chargeInfos)) {
            final ChargeInfo chargeInfo = chargeInfos.get(0);
            if (chargeInfo.getSlowVolt() == 0 && chargeInfo.getSlowPower() == 0
                    && chargeInfo.getSlow_free() == 0 && chargeInfo.getSlow_total() == 0) {
                mViewBinding.scenePoiDetailsChargingStationView.poiChargeSlowLayout.setVisibility(GONE);
            } else {
                mViewBinding.scenePoiDetailsChargingStationView.poiChargeSlowLayout.
                        setVisibility(VISIBLE);
            }
            if (chargeInfo.getFastVolt() == 0 && chargeInfo.getFastPower() == 0
                    && chargeInfo.getFast_free() == 0 && chargeInfo.getFast_total() == 0) {
                mViewBinding.scenePoiDetailsChargingStationView.poiChargeFastLayout.setVisibility(GONE);
            } else {
                mViewBinding.scenePoiDetailsChargingStationView.poiChargeFastLayout.
                        setVisibility(VISIBLE);
            }
            final String fastFree = chargeInfo.getFast_free() == 0 ?
                    DEFATULE_STRING : chargeInfo.getFast_free() + "";
            final String fastTotal = chargeInfo.getFast_total() == 0 ?
                    DEFATULE_STRING : "/" + chargeInfo.getFast_total();
            final String fastVolt = chargeInfo.getFastVolt() == 0 ?
                    DEFATULE_STRING : chargeInfo.getFastVolt() + "v";
            final String fastPower = chargeInfo.getFastPower() == 0 ?
                    DEFATULE_STRING : chargeInfo.getFastPower() + "kw";
            final String fastInfo = fastPower + "." + fastVolt;
            mViewBinding.scenePoiDetailsChargingStationView.poiChargeFastOccupied.setText(fastFree);
            mViewBinding.scenePoiDetailsChargingStationView.poiChargeFastTotal.setText(fastTotal);
            if(chargeInfo.getFast_free() == 0){
                mViewBinding.scenePoiDetailsChargingStationView.poiChargeFastOccupied.setVisibility(GONE);
                mViewBinding.scenePoiDetailsChargingStationView.poiChargeFastTotal.setText(fastTotal.replace("/",""));
            }

            mViewBinding.scenePoiDetailsChargingStationView.poiChargeFastCurrentAndVlot.
                    setText(fastInfo);
            final String slowFree = chargeInfo.getSlow_free() == 0 ?
                    DEFATULE_STRING : chargeInfo.getSlow_free() + "";
            final String slowTotal = chargeInfo.getSlow_total() == 0 ?
                    DEFATULE_STRING : "/" + chargeInfo.getSlow_total();
            final String slowVolt = chargeInfo.getSlowVolt() == 0 ?
                    DEFATULE_STRING : chargeInfo.getSlowVolt() + "v";
            final String slowPower = chargeInfo.getSlowPower() == 0 ?
                    DEFATULE_STRING : chargeInfo.getSlowPower() + "kw";
            final String slowInfo = slowPower + "." + slowVolt;
            mViewBinding.scenePoiDetailsChargingStationView.poiChargeSlowOccupied.setText(slowFree);
            mViewBinding.scenePoiDetailsChargingStationView.poiChargeSlowTotal.setText(slowTotal);
            if(chargeInfo.getSlow_free() == 0){
                mViewBinding.scenePoiDetailsChargingStationView.poiChargeSlowOccupied.setVisibility(GONE);
                mViewBinding.scenePoiDetailsChargingStationView.poiChargeSlowTotal.setText(slowTotal.replace("/",""));
            }
            mViewBinding.scenePoiDetailsChargingStationView.poiChargeSlowCurrentAndVlot.
                    setText(slowInfo);
            mViewBinding.scenePoiDetailsChargingStationView.poiChargePrice.setText(
                    getContext().getString(
                            R.string.charge_price, ConvertUtils.stringFormatTwo(chargeInfo.getCurrentElePrice())));
            mViewBinding.scenePoiDetailsChargingStationView.poiChargeParkPrice.setText(
                    getContext().getString(
                            R.string.charge_park_price, chargeInfo.getCurrentServicePrice()));
            if(mSearchResultEntity.getIsNetData()){
                mViewBinding.scenePoiDetailsChargingStationView.poiChargePriceAllday.setVisibility(VISIBLE);
                mViewBinding.scenePoiDetailsChargingStationView.poiChargeAppointment.setVisibility(VISIBLE);
            }else{
                mViewBinding.scenePoiDetailsChargingStationView.poiChargePriceAllday.setVisibility(GONE);
                mViewBinding.scenePoiDetailsChargingStationView.poiChargeAppointment.setVisibility(GONE);
            }
        }
        if (ConvertUtils.isEmpty(mPoiInfoEntity.getBusinessTime())) {
            mViewBinding.scenePoiDetailsChargingStationView.poiChargeBusinessLayout.setVisibility(View.GONE);
        } else {
            final String[] businessTimes = mPoiInfoEntity.getBusinessTime().split("；");
            final String text = getContext().getString(R.string.business_hour, businessTimes[0]);

            initPoiBusinessTimeIconObserver(mViewBinding.scenePoiDetailsChargingStationView.poiBusinessIcon,
                    mViewBinding.scenePoiDetailsChargingStationView.poiCharegBusinessHours, businessTimes.length > 1, businessTimes);
            mViewBinding.scenePoiDetailsChargingStationView.poiCharegBusinessHours.setText(text);
        }
        if (ConvertUtils.isEmpty(mPoiInfoEntity.getPhone())) {
            mViewBinding.scenePoiDetailsChargingStationView.poiChargeAreaPhone.
                    setVisibility(View.GONE);
        }
        final String[] phones = mPoiInfoEntity.getPhone().split(";");
        final String text = getContext().getString(R.string.poi_phone, phones[0]);
        initPoiPhoneIconObserver(mViewBinding.scenePoiDetailsChargingStationView.poiPhoneIcon,
                mViewBinding.scenePoiDetailsChargingStationView.poiChargeAreaPhone,
                phones.length > 1, phones);
        mViewBinding.scenePoiDetailsChargingStationView.poiChargeAreaPhone.setText(text);
        mViewBinding.scenePoiDetailsChargingStationView.poiChargePriceAllday.setOnClickListener(new OnClickListener() {
            @Override
            public void onClick(View view) {
                final Fragment fragment = (Fragment) ARouter.getInstance().build(
                        RoutePath.Search.POI_CHARGE_PRICE_ALL_DAY_FRAGMENT).navigation();
                addFragment((BaseFragment) fragment, SearchFragmentFactory.createChargePriceFragment(mPoiInfoEntity));
            }
        });
        mViewBinding.scenePoiDetailsChargingStationView.poiChargeFastLayout.setOnClickListener(v ->{
            toReservationListView(AutoMapConstant.EquipmentType.FAST);
        });
        mViewBinding.scenePoiDetailsChargingStationView.poiChargeSlowLayout.setOnClickListener(v ->{
            toReservationListView(AutoMapConstant.EquipmentType.SLOW);
        });
        if(!ConvertUtils.isNull(mPoiInfoEntity.getReservationInfo())){
            mViewBinding.scenePoiDetailsChargingStationView.poiChargeAppointment.setText(R.string.charge_appointment_look);
        }else{
            mViewBinding.scenePoiDetailsChargingStationView.poiChargeAppointment.setText(R.string.charge_appointment);
        }
        mViewBinding.scenePoiDetailsChargingStationView.poiChargeAppointment.setOnClickListener(v -> {
            if(!ConvertUtils.isNull(mPoiInfoEntity.getReservationInfo())){
                toReservationDetailView();
            }else{
                toReservationListView(-1);
            }
        });
        final String imageUrl = mPoiInfoEntity.getImageUrl();
        if(ConvertUtils.isNull(imageUrl)){
            mViewBinding.scenePoiDetailsChargingStationView.poiChargeImgLayout.setVisibility(GONE);
        }else{
            mViewBinding.scenePoiDetailsChargingStationView.poiChargeImgLayout.setVisibility(VISIBLE);
            ViewAdapterKt.loadImageUrl(mViewBinding.scenePoiDetailsChargingStationView.poiChargeImg,
                    imageUrl, R.drawable.test_pic, R.drawable.test_pic);
        }
        mViewBinding.scenePoiDetailsGasStationView.poiGasRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsChargingStationView.poiChargeRoot.setVisibility(VISIBLE);
        mViewBinding.scenePoiDetailsWashCarView.poiWashCarRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsCateringView.poiCateringRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsServiceAreaView.poiServiceAreaRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsNormalView.poiNormalRoot.setVisibility(GONE);
    }

    /**
     * 刷新洗车视图
     */
    private void refreshCarWashView() {
        mViewBinding.scenePoiDetailsGasStationView.poiGasRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsChargingStationView.poiChargeRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsWashCarView.poiWashCarRoot.setVisibility(VISIBLE);
        mViewBinding.scenePoiDetailsCateringView.poiCateringRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsServiceAreaView.poiServiceAreaRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsNormalView.poiNormalRoot.setVisibility(GONE);
    }

    /**
     * 刷新餐饮信息
     */
    private void refreshCateringView() {
        final String rating = mPoiInfoEntity.getRating();
        if (TextUtils.isEmpty(rating)) {
            mViewBinding.scenePoiDetailsCateringView.poiCateringMarkValue.setVisibility(GONE);
            mViewBinding.scenePoiDetailsCateringView.poiCateringMarkLayout.setVisibility(GONE);
        } else {
            mViewBinding.scenePoiDetailsCateringView.poiCateringMarkValue.setText(rating);
            final float realRating = Float.parseFloat(rating);
            if (realRating >= 0.5) {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg1.setImageResource(
                        R.drawable.img_collect_48);
            } else if (realRating < 0.5 && realRating > 0) {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg1.setImageResource(
                        R.drawable.img_collect2_48);
            } else {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg1.setImageResource(
                        R.drawable.img_collect3_42);
            }
            if (realRating >= 1.5) {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg2.setImageResource(
                        R.drawable.img_collect_48);
            } else if (realRating < 1.5 && realRating > 1) {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg2.setImageResource(
                        R.drawable.img_collect2_48);
            } else {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg2.setImageResource(
                        R.drawable.img_collect3_42);
            }
            if (realRating >= 2.5) {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg3.setImageResource(
                        R.drawable.img_collect_48);
            } else if (realRating < 2.5 && realRating > 2) {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg3.setImageResource(
                        R.drawable.img_collect2_48);
            } else {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg3.setImageResource(
                        R.drawable.img_collect3_42);
            }
            if (realRating >= 3.5) {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg4.setImageResource(
                        R.drawable.img_collect_48);
            } else if (realRating < 3.5 && realRating > 3) {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg4.setImageResource(
                        R.drawable.img_collect2_48);
            } else {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg4.setImageResource(
                        R.drawable.img_collect3_42);
            }
            if (realRating >= 4.5) {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg5.setImageResource(
                        R.drawable.img_collect_48);
            } else if (realRating < 4.5 && realRating > 4) {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg5.setImageResource(
                        R.drawable.img_collect2_48);
            } else {
                mViewBinding.scenePoiDetailsCateringView.poiCateringMarkImg5.setImageResource(
                        R.drawable.img_collect3_42);
            }
        }
        final String avgCost;
        if (mPoiInfoEntity.getAverageCost() == -1) {
            avgCost = DEFATULE_STRING;
            mViewBinding.scenePoiDetailsCateringView.poiCateringPricePerPerson.setVisibility(GONE);
        } else {
            avgCost = getContext().getString(R.string.catering_price, mPoiInfoEntity.getAverageCost());
            mViewBinding.scenePoiDetailsCateringView.poiCateringPricePerPerson.
                    setVisibility(VISIBLE);
        }
        if (ConvertUtils.isEmpty(mPoiInfoEntity.getBusinessTime())) {
            mViewBinding.scenePoiDetailsCateringView.poiCateringHoursLayout.setVisibility(View.GONE);
        } else {
            final String[] businessTimes = mPoiInfoEntity.getBusinessTime().split("；");
            final String text = getContext().getString(R.string.business_hour, businessTimes[0]);

            initPoiBusinessTimeIconObserver(mViewBinding.scenePoiDetailsCateringView.poiBusinessIcon,
                    mViewBinding.scenePoiDetailsCateringView.poiCateringHours, businessTimes.length > 1, businessTimes);
            mViewBinding.scenePoiDetailsCateringView.poiCateringHours.setText(text);
        }
        if (ConvertUtils.isEmpty(mPoiInfoEntity.getPhone())) {
            mViewBinding.scenePoiDetailsCateringView.poiCateringPhone.setVisibility(View.GONE);
        }

        final String[] phones = mPoiInfoEntity.getPhone().split(";");
        final String text = getContext().getString(R.string.poi_phone, phones[0]);
        initPoiPhoneIconObserver(mViewBinding.scenePoiDetailsCateringView.poiPhoneIcon,
                mViewBinding.scenePoiDetailsCateringView.poiCateringPhone,
                phones.length > 1, phones);
        mViewBinding.scenePoiDetailsCateringView.poiCateringPhone.setText(text);
        mViewBinding.scenePoiDetailsCateringView.poiCateringPrice.setText(avgCost);
        mViewBinding.scenePoiDetailsGasStationView.poiGasRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsChargingStationView.poiChargeRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsWashCarView.poiWashCarRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsCateringView.poiCateringRoot.setVisibility(VISIBLE);
        mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsServiceAreaView.poiServiceAreaRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsNormalView.poiNormalRoot.setVisibility(GONE);
    }

    /**
     * 刷新停车场视图
     */
    private void refreshParkingLotView() {
        if (mPoiInfoEntity == null || mPoiInfoEntity.getParkingInfoList() == null
                || mPoiInfoEntity.getParkingInfoList().isEmpty()) {
            return;
        }
        final ParkingInfo parkingInfo = mPoiInfoEntity.getParkingInfoList().get(0);
        String parkString = "";
        final int spaceFree = parkingInfo.getSpaceFree();
        final int spaceTotal = parkingInfo.getSpaceTotal();
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "spaceFree :" + spaceFree + " spaceTotal :" + spaceTotal);
        if (spaceFree == -1 && spaceTotal == -1) {
            mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotOccupied.setVisibility(GONE);
        } else if (spaceFree == -1) {
            parkString = getContext().getString(R.string.parking_lot_total, spaceTotal);
            mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotOccupied.setText(parkString);
            mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotOccupied.setTextColor(
                    ResourceUtils.Companion.getInstance().getColor(R.color.search_loading_bg_80));
        } else {
            parkString = getContext().getString(R.string.parking_lot_status, spaceFree, spaceTotal);
            mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotOccupied.setText(parkString);
        }
        mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotOccupied.setText(parkString);
        if (ConvertUtils.isEmpty(mPoiInfoEntity.getBusinessTime())) {
            mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotHoursLayout.setVisibility(View.GONE);
        } else {
            final String[] businessTimes = mPoiInfoEntity.getBusinessTime().split("；");
            final String text = getContext().getString(R.string.business_hour, businessTimes[0]);

            initPoiBusinessTimeIconObserver(mViewBinding.scenePoiDetailsParkingLotView.poiBusinessIcon,
                    mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotHours, businessTimes.length > 1, businessTimes);
            mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotHours.setText(text);
        }
        if (ConvertUtils.isEmpty(mPoiInfoEntity.getPhone())) {
            mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotPhone.setVisibility(View.GONE);
        }
        final String[] phones = mPoiInfoEntity.getPhone().split(";");
        final String text = getContext().getString(R.string.poi_phone, phones[0]);
        initPoiPhoneIconObserver(mViewBinding.scenePoiDetailsParkingLotView.poiPhoneIcon,
                mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotPhone,
                phones.length > 1, phones);
        mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotPhone.setText(text);
        mViewBinding.scenePoiDetailsGasStationView.poiGasRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsChargingStationView.poiChargeRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsWashCarView.poiWashCarRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsCateringView.poiCateringRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotRoot.setVisibility(VISIBLE);
        mViewBinding.scenePoiDetailsServiceAreaView.poiServiceAreaRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsNormalView.poiNormalRoot.setVisibility(GONE);
    }

    /**
     * 刷新服务区视图
     */
    private void refreshServiceAreaView() {
        mViewBinding.scenePoiDetailsServiceAreaView.poiServiceAreaFacility.setVisibility(GONE);
        final List<GasStationInfo> gasStationInfos = mPoiInfoEntity.getStationList();
        if (gasStationInfos == null || gasStationInfos.isEmpty()) {
            mViewBinding.scenePoiDetailsServiceAreaView.poiServiceAreaOil.setVisibility(GONE);
        } else {
            final LinearLayoutManager layoutManager1 = new LinearLayoutManager(getContext());
            layoutManager1.setOrientation(LinearLayoutManager.HORIZONTAL);
            mViewBinding.scenePoiDetailsServiceAreaView.routePoidetailGasStation.
                    setLayoutManager(layoutManager1);
            final RoutePOIGasStationAdapter serviceGasAdapter = new RoutePOIGasStationAdapter();
            mViewBinding.scenePoiDetailsServiceAreaView.routePoidetailGasStation.setAdapter(
                    serviceGasAdapter);
            final List<String> gasString = new ArrayList<>();
            for (GasStationInfo gasStationInfo : gasStationInfos) {
                gasString.add(gasStationInfo.getType());
            }
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "gasString = 1" + gasString + " size: "
                    + gasStationInfos.size());
            serviceGasAdapter.setRouteBeanList(gasString);
        }
        if (ConvertUtils.isEmpty(mPoiInfoEntity.getBusinessTime())) {
            mViewBinding.scenePoiDetailsServiceAreaView.poiServiceAreaHoursLayout.setVisibility(View.GONE);
        } else {
            final String[] businessTimes = mPoiInfoEntity.getBusinessTime().split("；");
            final String text = getContext().getString(R.string.business_hour, businessTimes[0]);

            initPoiBusinessTimeIconObserver(mViewBinding.scenePoiDetailsServiceAreaView.poiBusinessIcon,
                    mViewBinding.scenePoiDetailsServiceAreaView.poiServiceAreaHours, businessTimes.length > 1, businessTimes);
            mViewBinding.scenePoiDetailsServiceAreaView.poiServiceAreaHours.setText(text);
        }
        if (ConvertUtils.isEmpty(mPoiInfoEntity.getPhone())) {
            mViewBinding.scenePoiDetailsServiceAreaView.poiServiceAreaPhone.
                    setVisibility(View.GONE);
        }

        final String[] phones = mPoiInfoEntity.getPhone().split(";");
        final String text = getContext().getString(R.string.poi_phone, phones[0]);
        initPoiPhoneIconObserver(mViewBinding.scenePoiDetailsServiceAreaView.poiPhoneIcon,
                mViewBinding.scenePoiDetailsServiceAreaView.poiServiceAreaPhone,
                phones.length > 1, phones);
        mViewBinding.scenePoiDetailsServiceAreaView.poiServiceAreaPhone.setText(text);
        mViewBinding.scenePoiDetailsGasStationView.poiGasRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsChargingStationView.poiChargeRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsWashCarView.poiWashCarRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsCateringView.poiCateringRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsServiceAreaView.poiServiceAreaRoot.setVisibility(VISIBLE);
        mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsNormalView.poiNormalRoot.setVisibility(GONE);
    }

    /**
     * 刷新子项列表
     */
    private void refreshChildListView() {
        final List<ChildInfo> childInfoList = mPoiInfoEntity.getChildInfoList();
        mScenicChildAdapter = new PoiDetailsScenicChildAdapter();
        if (childInfoList != null && !childInfoList.isEmpty()) {
            for (ChildInfo childInfo : childInfoList) {
                mScreenViewModel.setGrandChildInfoList(childInfo)
                        .thenAccept(childInfoNew -> {
                            if (!ConvertUtils.isEmpty(childInfoNew.getMGrandChildInfoList())) {
                                mPoiInfoEntity.setMChildType(AutoMapConstant.ChildType.HAS_CHILD_HAS_GRAND);
                            }
                            childInfo.setMGrandChildInfoList(childInfoNew.getMGrandChildInfoList());
                            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "ChildList " + GsonUtils.toJson(childInfo));
                        })
                        .exceptionally(error -> {
                            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "setGrandChildInfoList error:" + error);
                            return null;
                        });
            }


            if (mChildSelectIndex != -1 && mChildSelectIndex < childInfoList.size()) {
                childInfoList.get(mChildSelectIndex).setChecked(1);
                final ChildInfo childInfo = childInfoList.get(mChildSelectIndex);
                mChildSelectInfo = new PoiInfoEntity()
                        .setName(childInfo.getName())
                        .setAddress(childInfo.getAddress())
                        .setPid(childInfo.getPoiId())
                        .setMChildInfoList(childInfo.getMGrandChildInfoList())
                        .setPoint(childInfo.getLocation());
            }
            mScenicChildAdapter.setChildInfoList(childInfoList);
            mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotChildList.setVisibility(View.VISIBLE);
            mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotChildList.setLayoutManager(
                    new GridLayoutManager(getContext(), mSpanCount));
            mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotChildList.addItemDecoration(
                    new GridSpacingItemDecoration(getContext(), mSpanCount, mChildSpacing,
                            mChildSpacing, false));
            mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotChildList.
                    setAdapter(mScenicChildAdapter);
            if (childInfoList.size() > 2) {
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotChildExpandCollapse.
                        setVisibility(VISIBLE);
            } else {
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotChildExpandCollapse.
                        setVisibility(GONE);
            }
            mScenicChildAdapter.setItemClickListener((index, isSelectIndex) -> {
                if (isSelectIndex) {
                    final ChildInfo childInfo = childInfoList.get(index);
                    mChildSelectInfo = new PoiInfoEntity()
                            .setName(childInfo.getName())
                            .setAddress(childInfo.getAddress())
                            .setPid(childInfo.getPoiId())
                            .setMChildInfoList(childInfo.getMGrandChildInfoList())
                            .setPoint(childInfo.getLocation());
                    mScreenViewModel.setChildIndex(index);
                    if (ConvertUtils.isEmpty(childInfo.getMGrandChildInfoList())) {
                        mChildSelectInfo.setMChildType(AutoMapConstant.ChildType.CHILD_NO_GRAND);
                        mViewBinding.lySecondaryPoi.setVisibility(View.GONE);
                    } else {
                        mChildSelectInfo.setMChildType(AutoMapConstant.ChildType.CHILD_HAS_GRAND);
                        mViewBinding.lySecondaryPoi.setVisibility(View.VISIBLE);
                        mViewBinding.lySecondaryPoi.setUIMode(AutoMapConstant.ChildType.CHILD_HAS_GRAND, mChildSelectInfo);
                        mViewBinding.lySecondaryPoi.setItemClickListener(new SceneRouteDescendantsView.OnItemClickListener() {
                            @Override
                            public void onItemClick(final PoiInfoEntity poiInfo) {
                                mGrandChildSelectInfo = poiInfo;
                            }

                            @Override
                            public void onCancelSelectClick(final PoiInfoEntity poiInfoEntity) {
                                mGrandChildSelectInfo = null;
                            }

                            @Override
                            public void OnScrollListener() {
                            }
                        });
                    }
                } else {
                    mChildSelectInfo = null;
                    mViewBinding.lySecondaryPoi.setVisibility(View.GONE);
                }
                mGrandChildSelectInfo = null;
                refreshPoiView(mPoiType,mPoiInfoEntity,false);
            });
        } else {
            mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotChildList.setVisibility(View.GONE);
            mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotChildExpandCollapse.
                    setVisibility(GONE);
        }
        mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotChildExpandCollapse.
                setOnClickListener(v -> {
                    mScenicChildAdapter.setCollapse(!mScenicChildAdapter.isCollapse());
                    mScenicChildAdapter.notifyDataSetChanged();
                    mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotChildExpandCollapse.
                            setImageResource(mScenicChildAdapter.isCollapse() ?
                                    R.drawable.img_under_the_48 : R.drawable.img_up_48);
                });
        mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotChildExpandCollapse.
                setImageResource(mScenicChildAdapter.isCollapse() ? R.drawable.img_under_the_48 :
                        R.drawable.img_up_48);
    }

    /**
     * 刷新景点视图
     */
    private void refreshScenicSpotView() {
        refreshChildListView();
        final String hourTime =  mPoiInfoEntity.getBusinessTime();
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "hourTime : " + hourTime);

        final String rating = mPoiInfoEntity.getRating();
        if (TextUtils.isEmpty(rating)) {
            mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkLayout.setVisibility(GONE);
            mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkValue.setVisibility(GONE);
        } else {
            mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkValue.setText(rating);
            final float realRating = Float.parseFloat(rating);
            if (realRating >= 0.5) {
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg1.setImageResource(
                        R.drawable.img_collect_48);
            } else if (realRating < 0.5 && realRating > 0){
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg1.setImageResource(
                        R.drawable.img_collect2_48);
            } else {
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg1.setImageResource(
                        R.drawable.img_collect3_42);
            }
            if (realRating >= 1.5) {
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg2.setImageResource(
                        R.drawable.img_collect_48);
            } else if (realRating < 1.5 && realRating > 1){
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg2.setImageResource(
                        R.drawable.img_collect2_48);
            } else {
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg2.setImageResource(
                        R.drawable.img_collect3_42);
            }
            if (realRating >= 2.5) {
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg3.setImageResource(
                        R.drawable.img_collect_48);
            } else if (realRating < 2.5 && realRating > 2){
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg3.setImageResource(
                        R.drawable.img_collect2_48);
            } else {
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg3.setImageResource(
                        R.drawable.img_collect3_42);
            }
            if (realRating >= 3.5) {
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg4.setImageResource(
                        R.drawable.img_collect_48);
            } else if (realRating < 3.5 && realRating > 3){
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg4.setImageResource(
                        R.drawable.img_collect2_48);
            } else {
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg4.setImageResource(
                        R.drawable.img_collect3_42);
            }
            if (realRating >= 4.5) {
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg5.setImageResource(
                        R.drawable.img_collect_48);
            } else if (realRating < 4.5 && realRating > 4){
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg5.setImageResource(
                        R.drawable.img_collect2_48);
            } else {
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotMarkImg5.setImageResource(
                        R.drawable.img_collect3_42);
            }
        }
        final String avgCost;
        if (mPoiInfoEntity.getAverageCost() == -1 || mPoiInfoEntity.getAverageCost() == 0) {
            mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotPrice.setText("--");
        } else {
            avgCost = getContext().getString(R.string.catering_price, mPoiInfoEntity.getAverageCost());
            mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotPrice.setText(avgCost);
        }
        if (ConvertUtils.isEmpty(mPoiInfoEntity.getBusinessTime())) {
            mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotHoursLayout.setVisibility(View.GONE);
        } else {
            final String[] businessTimes = mPoiInfoEntity.getBusinessTime().split("；");
            final String text = getContext().getString(R.string.business_hour, businessTimes[0]);
            initPoiBusinessTimeIconObserver(mViewBinding.scenePoiDetailsScenicSpotView.poiBusinessIcon,
                    mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotHoursContent, businessTimes.length > 1, businessTimes);
            mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotHoursContent.setText(text);
        }
        if (ConvertUtils.isEmpty(mPoiInfoEntity.getPhone())) {
            mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotPhone.setVisibility(View.GONE);
        }

        final String[] phones = mPoiInfoEntity.getPhone().split(";");
        final String text = getContext().getString(R.string.poi_phone, phones[0]);
        initPoiPhoneIconObserver(mViewBinding.scenePoiDetailsScenicSpotView.poiPhoneIcon,
                mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotPhone,
                phones.length > 1, phones);
        mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotPhone.setText(text);

        mViewBinding.scenePoiDetailsGasStationView.poiGasRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsChargingStationView.poiChargeRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsWashCarView.poiWashCarRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsCateringView.poiCateringRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsServiceAreaView.poiServiceAreaRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotRoot.setVisibility(VISIBLE);
        mViewBinding.scenePoiDetailsNormalView.poiNormalRoot.setVisibility(GONE);
    }

    /**
     * 刷新通用视图
     */
    private void refreshNormalView() {
        mViewBinding.scenePoiDetailsGasStationView.poiGasRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsChargingStationView.poiChargeRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsWashCarView.poiWashCarRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsCateringView.poiCateringRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsParkingLotView.poiParkingLotRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsServiceAreaView.poiServiceAreaRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsScenicSpotView.poiScenicSpotRoot.setVisibility(GONE);
        mViewBinding.scenePoiDetailsNormalView.poiNormalRoot.setVisibility(VISIBLE);
        if (mPoiInfoEntity == null) {
            return;
        }
        final List<ChildInfo> childInfoList = mPoiInfoEntity.getChildInfoList();
        final PoiDetailsScenicChildAdapter scenicChildAdapter = new PoiDetailsScenicChildAdapter();
        if (childInfoList != null && !childInfoList.isEmpty()) {
            for (ChildInfo childInfo : childInfoList) {
                mScreenViewModel.setGrandChildInfoList(childInfo)
                        .thenAccept(childInfoNew -> {
                            if (!ConvertUtils.isEmpty(childInfoNew.getMGrandChildInfoList())) {
                                mPoiInfoEntity.setMChildType(AutoMapConstant.ChildType.HAS_CHILD_HAS_GRAND);
                            }
                            childInfo.setMGrandChildInfoList(childInfoNew.getMGrandChildInfoList());
                            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "ChildList " + GsonUtils.toJson(childInfo));
                        })
                        .exceptionally(error -> {
                            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "setGrandChildInfoList error:" + error);
                            return null;
                        });
            }
            scenicChildAdapter.setChildInfoList(childInfoList);
            mViewBinding.scenePoiDetailsNormalView.poiChildList.setLayoutManager(
                    new GridLayoutManager(getContext(), mSpanCount));
            if (mViewBinding.scenePoiDetailsNormalView.poiChildList.getItemDecorationCount() == 0) {
                mViewBinding.scenePoiDetailsNormalView.poiChildList.addItemDecoration(
                        new GridSpacingItemDecoration(getContext(), mSpanCount, mChildSpacing,
                                mChildSpacing, false));
            }
            mViewBinding.scenePoiDetailsNormalView.poiChildList.setAdapter(scenicChildAdapter);
            if (childInfoList.size() > 2) {
                mViewBinding.scenePoiDetailsNormalView.poiChildExpandCollapse.
                        setVisibility(VISIBLE);
            } else {
                mViewBinding.scenePoiDetailsNormalView.poiChildExpandCollapse.setVisibility(GONE);
            }
            scenicChildAdapter.setItemClickListener((index, isSelectIndex) -> {
                if (isSelectIndex) {
                    final ChildInfo childInfo = childInfoList.get(index);
                    mChildSelectInfo = new PoiInfoEntity()
                            .setName(childInfo.getName())
                            .setAddress(childInfo.getAddress())
                            .setPid(childInfo.getPoiId())
                            .setMChildInfoList(childInfo.getMGrandChildInfoList())
                            .setPoint(childInfo.getLocation());
                    if (ConvertUtils.isEmpty(childInfo.getMGrandChildInfoList())) {
                        mChildSelectInfo.setMChildType(AutoMapConstant.ChildType.CHILD_NO_GRAND);
                        mViewBinding.lySecondaryPoi.setVisibility(View.GONE);
                    } else {
                        mChildSelectInfo.setMChildType(AutoMapConstant.ChildType.CHILD_HAS_GRAND);
                        mViewBinding.lySecondaryPoi.setVisibility(View.VISIBLE);
                        mViewBinding.lySecondaryPoi.setUIMode(AutoMapConstant.ChildType.CHILD_HAS_GRAND, mChildSelectInfo);
                        mViewBinding.lySecondaryPoi.setItemClickListener(new SceneRouteDescendantsView.OnItemClickListener() {
                            @Override
                            public void onItemClick(final PoiInfoEntity poiInfo) {
                                mGrandChildSelectInfo = poiInfo;
                            }

                            @Override
                            public void onCancelSelectClick(final PoiInfoEntity poiInfoEntity) {
                                mGrandChildSelectInfo = null;
                            }

                            @Override
                            public void OnScrollListener() {
                            }
                        });
                    }
                    mScreenViewModel.setChildIndex(index);
                } else {
                    mChildSelectInfo = null;
                    mViewBinding.lySecondaryPoi.setVisibility(View.GONE);
                }
                mGrandChildSelectInfo = null;
                refreshPoiView(mPoiType,mPoiInfoEntity,false);
            });
        } else {
            mViewBinding.scenePoiDetailsNormalView.poiChildExpandCollapse.setVisibility(GONE);
        }
        mViewBinding.scenePoiDetailsNormalView.poiChildExpandCollapse.setOnClickListener(v -> {
            scenicChildAdapter.setCollapse(!scenicChildAdapter.isCollapse());
            scenicChildAdapter.notifyDataSetChanged();
            mViewBinding.scenePoiDetailsNormalView.poiChildExpandCollapse.setImageResource(
                    scenicChildAdapter.isCollapse() ?
                            R.drawable.img_under_the_48 : R.drawable.img_up_48);
        });
        mViewBinding.scenePoiDetailsNormalView.poiChildExpandCollapse.setImageResource(
                scenicChildAdapter.isCollapse() ?
                        R.drawable.img_under_the_48 : R.drawable.img_up_48);

    }

    @Override
    public void onDestroy() {
        if (mScreenViewModel != null) {
            mScreenViewModel.clearAllPoiMarker();
        }
        super.onDestroy();
        if (mAnimator != null) {
            mAnimator.cancel();
        }
        mChildSelectInfo = null;
        mGrandChildSelectInfo = null;
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onDestroy");
        ThreadManager.getInstance().removeHandleTask(mTimeoutTask);
    }

    private final Runnable mTimeoutTask = new Runnable() {
        @Override
        public void run() {
            if (!ConvertUtils.isEmpty(mViewBinding)) {
                mViewBinding.csPoiNoResult.setVisibility(View.VISIBLE);
                mViewBinding.noResultButton.setVisibility(View.VISIBLE);
                mViewBinding.noResultHint.setText(getContext().getString(R.string.load_failed));
                mViewBinding.ivLoading.setVisibility(View.GONE);
                mViewBinding.skPoiName.setVisibility(View.GONE);
                mViewBinding.poiDetailsScroll.setVisibility(View.GONE);
                mViewBinding.poiTypeIcon.setVisibility(View.GONE);
                mViewBinding.scenePoiDetailsBottomView.getRoot().setVisibility(View.GONE);
                if (mAnimator != null) {
                    mAnimator.cancel();
                }
                mViewBinding.noResultButton.setOnClickListener((view) -> {
                    doSearch(mPoiInfoEntity);
//                    mViewBinding.csPoiNoResult.setVisibility(View.GONE);
//                    mViewBinding.skPoiName.setVisibility(View.VISIBLE);
//                    mViewBinding.poiDetailsScroll.setVisibility(View.VISIBLE);
//                    mViewBinding.poiTypeIcon.setVisibility(View.VISIBLE);
//                    mViewBinding.scenePoiDetailsBottomView.getRoot().setVisibility(View.VISIBLE);
                });
            }
        }
    };

    /**
     * 是否显示加载动画
     * @param isShow 是否显示
     */
    private void showLoading(final boolean isShow) {
        mViewBinding.csPoiNoResult.setVisibility(isShow ? View.VISIBLE : View.GONE);
        mViewBinding.ivLoading.setVisibility(isShow ? View.VISIBLE : View.GONE);
        mViewBinding.noResultHint.setText(getContext().getString(R.string.address_loading));
        mViewBinding.noResultButton.setVisibility(View.GONE);
        if (mAnimator != null) {
            if (isShow) {
                mAnimator.start();
            } else {
                mAnimator.cancel();
            }
        }
        mViewBinding.skPoiName.setVisibility(isShow ? View.GONE : View.VISIBLE);
        mViewBinding.poiDetailsScroll.setVisibility(isShow ? View.GONE : View.VISIBLE);
        mViewBinding.poiTypeIcon.setVisibility(isShow ? View.GONE : View.VISIBLE);
        mViewBinding.scenePoiDetailsBottomView.getRoot().setVisibility(isShow ? View.GONE : View.VISIBLE);
    }

    /**
     * 执行搜索操作
     * @param poiInfo 搜索对象实体类
     */
    public void doSearch(final PoiInfoEntity poiInfo) {
        Logger.e(MapDefaultFinalTag.SEARCH_HMI_TAG, "doSearch ");
        showLoading(true);
        ThreadManager.getInstance().removeHandleTask(mTimeoutTask);
        ThreadManager.getInstance().postDelay(mTimeoutTask, 6000);
        mPoiInfoEntity = poiInfo;
        if(ConvertUtils.isEmpty(poiInfo.getOperatorId())){
            if(poiInfo.isIsLocres()){
                ArrayList<PoiInfoEntity> list = new ArrayList<>();
                list.add(poiInfo);
                SearchResultEntity searchResultEntity = new SearchResultEntity()
                        .setPoiList(list)
                        .setPoiType(1);
                onSearchResult(mScreenViewModel.getMTaskId(),searchResultEntity);
                reloadPoiLabelMarker();
            }else if(!ConvertUtils.isEmpty(poiInfo.getPid()) && poiInfo.getPid().startsWith("C")){
                mScreenViewModel.keywordSearch(poiInfo);
            }else{
                mScreenViewModel.doSearch(poiInfo);
            }
        }else{
            mScreenViewModel.doSearchByNet(poiInfo);
        }
        if (mPoiType == AutoMapConstant.PoiType.POI_MAP_CAR_CLICK && mViewBinding != null) {
            mViewBinding.skPoiName.setText(R.string.shc_my_point);
            mViewBinding.poiDistanceTime.setVisibility(View.GONE);
            mViewBinding.poiArrivalCapacity.setVisibility(View.GONE);
            mViewBinding.sivArrivalCapacity.setVisibility(View.GONE);
            mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setVisibility(View.VISIBLE);
            mViewBinding.scenePoiDetailsBottomView.stlGoFirst.setVisibility(View.GONE);
            mViewBinding.scenePoiDetailsBottomView.stlFunction.setVisibility(View.GONE);
        }
    }

    public void setChildIndex(final int index) {
        mChildSelectIndex = index;
    }

    /**
     * 退回到详情页面时，重新按照搜索结果列表进行扎标
     * @param poiInfoEntities 需要重新扎标的列表
     */
    public void reloadLastPoiMarker(final List<PoiInfoEntity> poiInfoEntities) {
        mScreenViewModel.addPoiMarker(poiInfoEntities, 0);
    }

    /**
     * 退回到详情页面时，重新按照POI详情页数据进行扎标
     */
    public void reloadPoiLabelMarker() {
        mScreenViewModel.createLabelMarker(mSearchResultEntity);
    }

    /**
     * 刷新poi视图
     *
     * @param poiType poi类型
     *                POI_SUGGESTION = 0; // 预搜索
     *                POI_KEYWORD = 1; // 关键字搜索
     *                POI_HOME = 2; // 添加家
     *                POI_COMPANY = 3; // 添加公司
     *                POI_COLLECTION = 4; // 添加收藏地址
     *                POI_COMMON = 5; // 添加常用地址
     *                POI_AROUND = 6; // 添加途径点
     *                POI_MAP_CLICK = 7; // 地图点击
     *                POI_MAP_CAR_CLICK = 9;//自车位点击事件
     * @param poiInfoEntity poi信息
     * @param isNeedRefreshNormalView 是否需要刷新NormalView
     */
    public void refreshPoiView(final int poiType, final PoiInfoEntity poiInfoEntity, final boolean isNeedRefreshNormalView) {
        if (mViewBinding == null) {
            return;
        }
        this.mPoiType = poiType;
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "poiType: " + poiType);
        final int pointTypeCode = mScreenViewModel.getPointTypeCode(poiInfoEntity.getPointTypeCode());
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"operatorId: "+poiInfoEntity.getOperatorId());
        if(ConvertUtils.isEmpty(poiInfoEntity.getOperatorId()) && isNeedRefreshNormalView){
            refreshNormalView();
        }
        //刷新View
        switch (poiType) {
            case AutoMapConstant.PoiType.POI_SUGGESTION:
            case AutoMapConstant.PoiType.POI_KEYWORD:
            case AutoMapConstant.PoiType.POI_MAP_CLICK:
            case AutoMapConstant.PoiType.POI_AROUND:
                if (mScreenViewModel.isAlongWaySearch() && !mIsEnd) {
                    Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "添加途径点");
                    mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setImageDrawable(null);
                    final PoiInfoEntity poiInfo;
                    if (mGrandChildSelectInfo != null) {
                        poiInfo = mGrandChildSelectInfo;
                    } else if (mChildSelectInfo != null) {
                        poiInfo = mChildSelectInfo;
                    } else {
                        poiInfo = mPoiInfoEntity;
                    }
                    if (RoutePackage.getInstance().isBelongRouteParam(MapType.MAIN_SCREEN_MAIN_MAP, poiInfo)) {
                        mViaAddType = false;
                        if (RoutePackage.getInstance().isStartOrEndRouteParam(MapType.MAIN_SCREEN_MAIN_MAP, poiInfo)) {
                            mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setAlpha(0.5f);
                            mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setClickable(false);
                        } else {
                            mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setAlpha(1.0f);
                            mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setClickable(true);
                        }
                        mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setText(R.string.st_along_way_point_delete);
                    } else {
                        mViaAddType = true;
                        mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setText(R.string.st_along_way_point_add);
                        // 达到最大途径点禁止点击并置灰
                        if(RoutePackage.getInstance().isMaxRouteParam(MapType.MAIN_SCREEN_MAIN_MAP)){
                            mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setAlpha(0.5f);
                            mViewBinding.scenePoiDetailsBottomView.stlGoFirst.setAlpha(0.5f);
                        }else{
                            mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setAlpha(1);
                            mViewBinding.scenePoiDetailsBottomView.stlGoFirst.setAlpha(1);
                        }
                    }

                    mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setVisibility(GONE);
                    mViewBinding.scenePoiDetailsBottomView.stlAroundSearch.setVisibility(GONE);
                    if (mScreenViewModel.getViaCount() >= 1 && mViaAddType) {
                        mViewBinding.scenePoiDetailsBottomView.stlPoiFavorites.setVisibility(GONE);
                        mViewBinding.scenePoiDetailsBottomView.stlGoFirst.setVisibility(VISIBLE);
                    } else {
                        mViewBinding.scenePoiDetailsBottomView.stlPoiFavorites.setVisibility(VISIBLE);
                        mViewBinding.scenePoiDetailsBottomView.stlGoFirst.setVisibility(GONE);
                    }
                } else {
                    Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "去这里");
                    mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setImageDrawable(
                            ContextCompat.getDrawable(getContext(),
                                    R.drawable.icon_details_bottom_go_here));
                    mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setAlpha(1.0f);
                    mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setText(R.string.st_go_here);
                    mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setVisibility(VISIBLE);
                    mViewBinding.scenePoiDetailsBottomView.stlAroundSearch.setVisibility(VISIBLE);
                    mViewBinding.scenePoiDetailsBottomView.stlPoiFavorites.setVisibility(VISIBLE);
                }
                if (poiType == AutoMapConstant.PoiType.POI_MAP_CLICK) {
                    //地图选点不需要展示电话和营业时间界面
                    mViewBinding.poiContentLayout.setVisibility(View.GONE);
                    mViewBinding.scenePoiDetailsBottomView.stlPhone.setVisibility(View.GONE);
                    mViewBinding.poiDetailsSubLine.setVisibility(View.GONE);
                }
                break;
            case AutoMapConstant.PoiType.POI_HOME:
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "设置为家");
                mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setImageDrawable(null);
                mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setText(R.string.st_add_home);
                mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setVisibility(View.GONE);
                mViewBinding.scenePoiDetailsBottomView.stlAroundSearch.setVisibility(GONE);
                mViewBinding.scenePoiDetailsBottomView.stlPoiFavorites.setVisibility(VISIBLE);
                break;
            case AutoMapConstant.PoiType.POI_COMPANY:
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "设置为公司");
                mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setImageDrawable(null);
                mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setText(R.string.st_add_company);
                mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setVisibility(View.GONE);
                mViewBinding.scenePoiDetailsBottomView.stlAroundSearch.setVisibility(GONE);
                mViewBinding.scenePoiDetailsBottomView.stlPoiFavorites.setVisibility(VISIBLE);
                break;
            case AutoMapConstant.PoiType.POI_COLLECTION:
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "添加");
                mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setImageDrawable(
                        ContextCompat.getDrawable(getContext(),
                                R.drawable.icon_details_bottom_go_here));
                mViewBinding.scenePoiDetailsBottomView.stlStartRoute.setClickable(mScreenViewModel.isFavorite(mPoiInfoEntity).isEmpty());
                mViewBinding.scenePoiDetailsBottomView.stlStartRoute.setAlpha(mScreenViewModel.isFavorite(mPoiInfoEntity).isEmpty() ? 1.0f : 0.5f);
                mViewBinding.scenePoiDetailsBottomView.stlStartRoute.setIsClickChangeColor(false);
                mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setText(R.string.st_collect_add);
                mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setVisibility(View.VISIBLE);
                mViewBinding.scenePoiDetailsBottomView.stlAroundSearch.setVisibility(GONE);
                mViewBinding.scenePoiDetailsBottomView.stlPoiFavorites.setVisibility(GONE);
                break;
            case AutoMapConstant.PoiType.POI_COMMON:
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "添加");
                mViewBinding.scenePoiDetailsBottomView.stlStartRoute.setClickable(!mScreenViewModel.isFrequentAddress(mPoiInfoEntity));
                mViewBinding.scenePoiDetailsBottomView.stlStartRoute.setAlpha(mScreenViewModel.isFrequentAddress(mPoiInfoEntity) ? 0.5f : 1.0f);
                mViewBinding.scenePoiDetailsBottomView.stlStartRoute.setIsClickChangeColor(false);
                mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setImageDrawable(null);
                mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setText(R.string.st_collect_add);
                mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setVisibility(View.GONE);
                mViewBinding.scenePoiDetailsBottomView.stlAroundSearch.setVisibility(GONE);
                mViewBinding.scenePoiDetailsBottomView.stlPoiFavorites.setVisibility(VISIBLE);
                break;
            case AutoMapConstant.PoiType.POI_DELETE_AROUND:
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "删除途径点");
                mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setImageDrawable(null);
                mViewBinding.scenePoiDetailsBottomView.stvStartRoute.setText(R.string.st_along_way_point_delete);
                mViewBinding.scenePoiDetailsBottomView.sivStartRoute.setVisibility(GONE);
                mViewBinding.scenePoiDetailsBottomView.stlAroundSearch.setVisibility(GONE);
                mViewBinding.scenePoiDetailsBottomView.stlPoiFavorites.setVisibility(VISIBLE);
                break;
            default:
                break;
        }
        //注册点击事件
        registerClickEvent(poiType);
    }

    /**
     * 注册界面点击事件
     * @param poiType POI详情页面类型
     */
    private void registerClickEvent(final int poiType) {
        mViewBinding.scenePoiDetailsBottomView.stlStartRoute.setOnClickListener(v -> {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onClick poiType: " + poiType);
            switch (poiType) {
                case AutoMapConstant.PoiType.POI_SUGGESTION:
                case AutoMapConstant.PoiType.POI_KEYWORD:
                case AutoMapConstant.PoiType.POI_AROUND:
                case AutoMapConstant.PoiType.POI_MAP_CLICK:
                    handleRouteClick();
                    break;
                case AutoMapConstant.PoiType.POI_HOME:
                case AutoMapConstant.PoiType.POI_COMPANY:
                case AutoMapConstant.PoiType.POI_COLLECTION:
                case AutoMapConstant.PoiType.POI_COMMON:
                    //  1，家  2，公司 3，常去地址  0，普通收藏点;
                    if (null != mPoiInfoEntity) {
                        final int commonName;
                        final String resultText = switch (poiType) {
                            case AutoMapConstant.PoiType.POI_HOME -> {
                                commonName = 1;
                                closeAllFragmentsUntilTargetFragment(HOME_COMPANY_FRAGMENT);
                                showCurrentFragment();
                                yield "设置家成功";
                            }
                            case AutoMapConstant.PoiType.POI_COMPANY -> {
                                commonName = 2;
                                closeAllFragmentsUntilTargetFragment(HOME_COMPANY_FRAGMENT);
                                showCurrentFragment();
                                yield "设置公司成功";
                            }
                            case AutoMapConstant.PoiType.POI_COMMON -> {
                                commonName = 3;
                                if (mScreenViewModel.isFrequentAddress(mPoiInfoEntity)) {
                                    yield "";
                                } else {
                                    closeAllFragmentsUntilTargetFragment(HOME_COMPANY_FRAGMENT);
                                    showCurrentFragment();
                                    yield "添加成功";
                                }
                            }
                            default -> {
                                commonName = 0;
                                if (!mScreenViewModel.isFavorite(mPoiInfoEntity).isEmpty()) {
                                    yield "";
                                } else {
                                    closeAllFragmentsUntilTargetFragment(HOME_COMPANY_FRAGMENT);
                                    showCurrentFragment();
                                    yield "添加成功";
                                }
                            }
                        };
                        if (ConvertUtils.isEmpty(resultText)) {
                            return;
                        }
                        final FavoriteInfo favoriteInfo = new FavoriteInfo();
                        favoriteInfo.setCommonName(commonName)
                                .setUpdateTime(new Date().getTime());
                        if (ConvertUtils.isEmpty(mPoiInfoEntity.getPid())) {
                            //逆地理搜索出的点无poiId，需自己拼接
                            mPoiInfoEntity.setPid(mPoiInfoEntity.getPoint().getLon() + "_"
                                    + mPoiInfoEntity.getPoint().getLat());
                        }
                        mPoiInfoEntity.setFavoriteInfo(favoriteInfo);
                        mScreenViewModel.addFavorite(mPoiInfoEntity, commonName);
                        sendBuryPointForAddFavorite(mPoiInfoEntity.getName(), commonName);
                        //收藏逻辑执行完成后，会回到来源页，所以需要取消扎标
                        mScreenViewModel.clearLabelMarker();
//                        BehaviorPackage.getInstance().addFavoriteData(mPoiInfoEntity, commonName);
                        SettingUpdateObservable.getInstance().onUpdateSyncTime();
                        ToastUtils.Companion.getInstance().showCustomToastView(resultText);
                    }
                    break;
                case AutoMapConstant.PoiType.POI_DELETE_AROUND:
                    RoutePackage.getInstance().removeVia(MapType.MAIN_SCREEN_MAIN_MAP,
                            mPoiInfoEntity, true);
                    break;
                case AutoMapConstant.PoiType.POI_MAP_CAR_CLICK:
                    handleFavoriteClick();
                    break;
                default:
                    break;
            }
        });
        mViewBinding.scenePoiDetailsBottomView.stlGoFirst.setOnClickListener((view) -> {
            if (mGrandChildSelectInfo != null) {
                if (SearchPackage.getInstance().isAlongWaySearch() && !RoutePackage.getInstance().isMaxRouteParam(MapType.MAIN_SCREEN_MAIN_MAP)) {
                    RoutePackage.getInstance().addViaPoint(MapType.MAIN_SCREEN_MAIN_MAP,
                            mGrandChildSelectInfo, 0);
                }
            } else if (mChildSelectInfo != null) {
                if (SearchPackage.getInstance().isAlongWaySearch() && !RoutePackage.getInstance().isMaxRouteParam(MapType.MAIN_SCREEN_MAIN_MAP)) {
                    RoutePackage.getInstance().addViaPoint(MapType.MAIN_SCREEN_MAIN_MAP,
                            mChildSelectInfo, 0);
                }
            } else {
                if (SearchPackage.getInstance().isAlongWaySearch() && !RoutePackage.getInstance().isMaxRouteParam(MapType.MAIN_SCREEN_MAIN_MAP)) {
                    RoutePackage.getInstance().addViaPoint(MapType.MAIN_SCREEN_MAIN_MAP,
                            mPoiInfoEntity, 0);
                }
            }
        });
    }

    /**
     * 格式化距离数组
     * @param distance 距离数据
     * @return 格式化后的数据
     */
    private String formatDistanceArrayInternal(final int distance) {
        final String[] distanceArray = ConvertUtils.formatDistanceArray(AppContext.getInstance().getMContext(), distance);
        return distanceArray[0] + distanceArray[1];
    }

    public void toReservationListView(int type){
        if(!mSearchResultEntity.getIsNetData()){
            return;
        }
        if(!mScreenViewModel.isSGMLogin()){
            mScreenViewModel.startSGMLogin();
            return;
        }
        if(!ConvertUtils.isEmpty(mEtaInfo)){
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"getTime: "+mEtaInfo.getTime());
           if(mEtaInfo.getTime() > 30){
               ToastUtils.Companion.getInstance().showCustomToastView(getContext().getString(R.string.travel_max_time));
               return;
           }
        }
        final Fragment fragment = (Fragment) ARouter.getInstance().build(
                RoutePath.Search.POI_CHARGE_RESERVATION_LIST_FRAGMENT).navigation();
        addFragment((BaseFragment) fragment, SearchFragmentFactory.createEquipmentListFragment(type,mPoiInfoEntity));
    }

    public void toReservationDetailView(){
        final Fragment fragment = (Fragment) ARouter.getInstance().build(
                RoutePath.Search.POI_CHARGE_RESERVATION_DETAILS_FRAGMENT).navigation();
        addFragment((BaseFragment) fragment, SearchFragmentFactory.createChargePriceFragment(mPoiInfoEntity));
    }

    public void setPowerType(final int powerType){
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"powerType: "+powerType);
        mScreenViewModel.mPowerType.setValue(powerType);
    }

    /**
     * @param b 是否从导航进入的搜索页面
     */
    public void setNaviControl(final boolean b) {
        mIsOpenFromNavi = b;
    }

    public boolean getIsOpenFromNavi() {
        return mIsOpenFromNavi;
    }

    public void setIsEnd(final boolean isEnd) {
        this.mIsEnd = isEnd;
    }

}
