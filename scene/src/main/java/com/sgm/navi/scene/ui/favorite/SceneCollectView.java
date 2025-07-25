package com.sgm.navi.scene.ui.favorite;


import android.app.Activity;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.constraintlayout.widget.ConstraintLayout;
import androidx.core.content.ContextCompat;
import androidx.fragment.app.Fragment;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.alibaba.android.arouter.launcher.ARouter;
import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.scene.BaseSceneView;
import com.sgm.navi.scene.BuildConfig;
import com.sgm.navi.scene.R;
import com.sgm.navi.scene.RoutePath;
import com.sgm.navi.scene.databinding.SceneCollectViewBinding;
import com.sgm.navi.scene.impl.favorite.SceneCollectViewImpl;
import com.sgm.navi.scene.impl.search.FavoriteManager;
import com.sgm.navi.scene.impl.search.SearchFragmentFactory;
import com.sgm.navi.scene.ui.adapter.CollectResultAdapter;
import com.sgm.navi.scene.ui.search.SearchConfirmDialog;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.user.account.AccessTokenParam;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.service.logicpaket.search.SearchPackage;
import com.sgm.navi.ui.base.BaseFragment;
import com.sgm.navi.ui.dialog.IBaseDialogClickListener;

import java.util.ArrayList;
import java.util.List;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * @Description: 收藏夹
 * @CreateDate: $ $
 */
public class SceneCollectView extends BaseSceneView<SceneCollectViewBinding, SceneCollectViewImpl> {

    //0 普通收藏 1 常用地址 3 收到的点
    private int mCollectionType = AutoMapConstant.CollectionType.COLLECTION;
    private int mHomeCompanyType = -1;
    private boolean mIsChargingCollect = false;
    private boolean mIsAsyncData = true;
    private final static String PATAC_ACTION_LOGIN = "patac.hmi.user.intent.action.LOGIN";
    private AccessTokenParam mParams;

    public SceneCollectView(@NonNull final Context context) {
        super(context);
    }

    private CollectResultAdapter mAdapter;

    public SceneCollectView(@NonNull final Context context, @Nullable final AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneCollectView(@NonNull final Context context, @Nullable final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected SceneCollectViewBinding createViewBinding(final LayoutInflater inflater, final ViewGroup viewGroup) {
        return SceneCollectViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneCollectViewImpl initSceneImpl() {
        return new SceneCollectViewImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setSceneCollectViewImpl(mScreenViewModel);
    }

    @Override
    protected void initObserver() {
        registerBroadcast();
        setupRecyclerView();
        setViewClick();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        mParams = getAccessTokenParam((Activity) getContext());
    }

    /**
     * 初始化RecyclerView
     */
    private void setupRecyclerView() {
        mViewBinding.rcvCollect.setLayoutManager(new LinearLayoutManager(getContext()));
        mAdapter = new CollectResultAdapter();
        mViewBinding.rcvCollect.setAdapter(mAdapter);
        mViewBinding.rcvCollect.setItemAnimator(null);
        mAdapter.setOnItemClickListener(new CollectResultAdapter.OnItemClickListener() {
            @Override
            public void onItemClick(final int position, final PoiInfoEntity poiInfoEntity) {

                final Fragment fragment = (Fragment) ARouter.getInstance()
                        .build(RoutePath.Search.POI_DETAILS_FRAGMENT)
                        .navigation();
                if (mCollectionType == AutoMapConstant.CollectionType.COMMON || mCollectionType == AutoMapConstant.CollectionType.GET_POINT) {
                    //常用地址/收到的点跳转进入 项目 应该是去详情页 按钮显示为添加
                    if (mHomeCompanyType == AutoMapConstant.HomeCompanyType.COMMON) {
                        addFragment((BaseFragment) fragment, SearchFragmentFactory.createPoiDetailsFragment(
                                AutoMapConstant.SourceFragment.FRAGMENT_COLLECTION, AutoMapConstant.PoiType.POI_COMMON, poiInfoEntity));
                    } else if (mHomeCompanyType == AutoMapConstant.HomeCompanyType.ALONG_WAY) {
                        addFragment((BaseFragment) fragment, SearchFragmentFactory.createPoiDetailsFragment(
                                AutoMapConstant.SourceFragment.FRAGMENT_COLLECTION, AutoMapConstant.PoiType.POI_AROUND, poiInfoEntity));
                    } else {
                        addFragment((BaseFragment) fragment, SearchFragmentFactory.createPoiDetailsFragment(
                                AutoMapConstant.SourceFragment.FRAGMENT_COLLECTION, AutoMapConstant.PoiType.POI_COLLECTION, poiInfoEntity));
                    }
                } else {
                    // 正常状态下收藏夹进去 项目 应该是去算路 POI_COLLECTION
                    addFragment((BaseFragment) fragment, SearchFragmentFactory.createPoiDetailsFragment(
                            AutoMapConstant.SourceFragment.FRAGMENT_COLLECTION, AutoMapConstant.PoiType.POI_HISTORY_LIST_CLICK, poiInfoEntity));
                }
            }

            @Override
            public void onNaviClick(final int position, final PoiInfoEntity poiInfoEntity) {
                if (mCollectionType == AutoMapConstant.CollectionType.COMMON || mCollectionType == AutoMapConstant.CollectionType.GET_POINT) {
                    Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onNaviClick: " + mCollectionType + " ,homeCompanyType: " + mHomeCompanyType);
                    if (mHomeCompanyType == AutoMapConstant.HomeCompanyType.ALONG_WAY) {
                        //如果是添加途径点，则改为添加途径点后跳转到路线规划页面
                        if (SearchPackage.getInstance().isAlongWaySearch()) {
                            if (RoutePackage.getInstance().isBelongRouteParam(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity)) {
                                RoutePackage.getInstance().removeVia(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity, true);
                            } else {
                                RoutePackage.getInstance().addViaPoint(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity);
                            }
                        }
//                        closeAllFragmentsUntilTargetFragment("MainAlongWaySearchFragment");
                    } else {
                        closeAllFragmentsUntilTargetFragment("HomeCompanyFragment");
                        showCurrentFragment();
                        //如果是常用地址/收到的点跳转的收藏界面，那么点击导航按钮，根据来源页面的HomeCompany类型收藏为收藏点/常去的点
                        ThreadManager.getInstance().runAsync(() -> FavoriteManager.getInstance().addFavorite(poiInfoEntity, mHomeCompanyType));
                    }
                } else {
                    if (SearchPackage.getInstance().isAlongWaySearch()) {
                        if (RoutePackage.getInstance().isBelongRouteParam(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity)) {
                            RoutePackage.getInstance().removeVia(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity, true);
                        } else {
                            RoutePackage.getInstance().addViaPoint(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity);
                        }
                    } else {
                        final Fragment fragment = (Fragment) ARouter.getInstance()
                                .build(RoutePath.Route.ROUTE_FRAGMENT)
                                .navigation();
                        addFragment((BaseFragment) fragment, SearchFragmentFactory.createRouteFragment(poiInfoEntity));
                    }
                }
            }

            @Override
            public void onListCleared() {
                mViewBinding.sllNoFavorite.setVisibility(View.VISIBLE);
                if (mCollectionType == AutoMapConstant.CollectionType.GET_POINT) {
                    mViewBinding.tvNoFavorite.setText(AppCache.getInstance().getMContext().getString(R.string.scv_not_have_receive));
                } else {
                    mViewBinding.tvNoFavorite.setText(AppCache.getInstance().getMContext().getString(R.string.scv_not_have_favorite));
                }
            }

            @Override
            public void updateCollectStatus(PoiInfoEntity poiInfoEntity) {
                mScreenViewModel.updateCollectStatus(mParams,poiInfoEntity);
            }
        });
    }

    /**
     * 设置点击事件
     */
    public void setViewClick() {
        mViewBinding.collectTitleBarView.ivClose.setOnClickListener(view -> mScreenViewModel.closeSearch());
        mViewBinding.collectTitleBarView.collectBarTextAdd.setOnClickListener(view -> {
            final Fragment fragment = (Fragment) ARouter.getInstance()
                    .build(RoutePath.Search.HOME_COMPANY_FRAGMENT)
                    .navigation();
//            addFragment((BaseFragment) fragment, SearchFragmentFactory.createSugFragment(
//            AutoMapConstant.SourceFragment.FRAGMENT_COLLECTION, AutoMapConstant.SearchType.SEARCH_SUGGESTION));
            addFragment((BaseFragment) fragment, SearchFragmentFactory.createHomeCompanyFragment(
                    AutoMapConstant.SourceFragment.MAIN_SEARCH_FRAGMENT,
                    AutoMapConstant.SearchType.SEARCH_KEYWORD, AutoMapConstant.HomeCompanyType.COLLECTION));
        });
        // 常用收藏夹
        mViewBinding.naviBroadcastStandard.setOnClickListener(view -> {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"naviBroadcastStandard click" + mIsChargingCollect);
            if (!mIsChargingCollect) {
                //如果已经选中，不刷新列表
                return;
            }
            mIsChargingCollect = false;
            hideEmptyView();
            mAdapter.notifyList(new ArrayList<>());
            final ArrayList<PoiInfoEntity> list = mScreenViewModel.getFavoriteListAsync();
            if (ConvertUtils.isEmpty(list)) {
                mViewBinding.sllNoFavorite.setVisibility(VISIBLE);
                if (mCollectionType == AutoMapConstant.CollectionType.GET_POINT) {
                    mViewBinding.tvNoFavorite.setText(AppCache.getInstance().getMContext().getString(R.string.scv_not_have_receive));
                } else {
                    mViewBinding.tvNoFavorite.setText(AppCache.getInstance().getMContext().getString(R.string.scv_not_have_favorite));
                }
            } else {
                mAdapter.notifyList(list);
            }
        });

        // 专属充电站
        mViewBinding.naviBroadcastLarge.setOnClickListener(view -> {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"naviBroadcastLarge click" + mIsChargingCollect);
            if (mIsChargingCollect) {
                return;
            }
            mIsChargingCollect = true;
            hideEmptyView();
            mAdapter.notifyList(new ArrayList<>());
            // 判断是否已登录
            if(!mScreenViewModel.isSGMLogin()){
                mViewBinding.sllNoSgm.setVisibility(VISIBLE);
                return;
            }
            if(mIsAsyncData){
                // 获取收藏数据
                mScreenViewModel.queryCollectStation(mParams);
            }else{
                mViewBinding.sllNoCharge.setVisibility(VISIBLE);
            }
        });

        // 登陆SGM
        mViewBinding.sllNoLogin.setOnClickListener(view -> {
            mScreenViewModel.startSGMLogin();
        });

        // 获取失败刷新
        mViewBinding.sllNoRefresh.setOnClickListener(view -> {
        });
    }

    /**
     * 设置适配器数据
     * @param data data
     */
    public void setAdapterData(final List<PoiInfoEntity> data,final boolean isChargeStation) {
        if(ConvertUtils.isEmpty(mViewBinding)){
            return;
        }
        boolean isChargeSelect = mViewBinding.naviBroadcastLarge.isChecked();
        if(BuildConfig.DEBUG){
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"charge isselect: " + isChargeSelect + "isChargeStation: "+isChargeStation);
        }
        // 保证搜索列表和搜索行为一致
        if((isChargeSelect && isChargeStation) || (!isChargeSelect && !isChargeStation)){
            mAdapter.notifyList(data);
            if (mViewBinding != null) {
                hideEmptyView();
                if (ConvertUtils.isEmpty(data)) {
                    mViewBinding.sllNoFavorite.setVisibility(View.VISIBLE);
                    if (mCollectionType == AutoMapConstant.CollectionType.GET_POINT) {
                        mViewBinding.tvNoFavorite.setText(AppCache.getInstance().getMContext().getString(R.string.scv_not_have_receive));
                    } else {
                        mViewBinding.tvNoFavorite.setText(AppCache.getInstance().getMContext().getString(R.string.scv_not_have_favorite));
                    }
                }
            }
        }
    }

    /**
     * 设置收藏类型
     * @param collectionType collectionType
     */
    public void setCollectionType(final int collectionType) {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "setCollectionType: " + collectionType);
        this.mCollectionType = collectionType;
        if(collectionType == AutoMapConstant.CollectionType.COMMON) {
            mViewBinding.collectTitleBarView.sclCollectAdd.setVisibility(View.GONE);
        }
        if(collectionType == AutoMapConstant.CollectionType.GET_POINT){
            mViewBinding.collectTitleBarView.collectBarTextView.setText(getResources().getString(R.string.shc_get_point));
            mViewBinding.collectTitleBarView.sclCollectAdd.setVisibility(View.GONE);
        }
        if (mAdapter != null) {
            mAdapter.setCollectionType(collectionType);
        }
    }

    public void setHomeCompanyType(final int homeCompanyType) {
        this.mHomeCompanyType = homeCompanyType;
        if (mAdapter != null) {
            mAdapter.setHomeCompanyType(mHomeCompanyType);
        }
    }

    /**
     * 设置来源Fragment
     * @param sourceFragment sourceFragment
     */
    public void setSourceFragment(final String sourceFragment) {
        if (ConvertUtils.equals(sourceFragment, AutoMapConstant.SourceFragment.FRAGMENT_MAIN_ALONG_WAY)) {
            mViewBinding.collectTitleBarView.sclCollectAdd.setVisibility(View.GONE);
        }
    }

    public void setChargeType(){
        if(mScreenViewModel.mPowerType.getValue() == 1 || mScreenViewModel.mPowerType.getValue() == 2){
            mIsChargingCollect = false;
            mViewBinding.naviBroadcastStandard.setChecked(true);
        }
    }

    public void setPowerType(final int powerType){
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"powerType: " + powerType);
        mScreenViewModel.mPowerType.setValue(powerType);
        if (mCollectionType == AutoMapConstant.CollectionType.COLLECTION) {
            if (mScreenViewModel.mPowerType != null) {
                mViewBinding.tvFavoriteSwitch.setVisibility((powerType == 1
                        || powerType == 2) ? VISIBLE : GONE);
            }

            ConstraintLayout.LayoutParams rcvParams = (ConstraintLayout.LayoutParams) mViewBinding.rcvCollect.getLayoutParams();
            ConstraintLayout.LayoutParams sllParams = (ConstraintLayout.LayoutParams) mViewBinding.sllNoFavorite.getLayoutParams();
            if (mViewBinding.tvFavoriteSwitch.getVisibility() == VISIBLE) {
                rcvParams.matchConstraintMaxHeight = getResources().getDimensionPixelSize(com.sgm.navi.ui.R.dimen.dp_745);
                sllParams.height = getResources().getDimensionPixelSize(com.sgm.navi.ui.R.dimen.dp_613);
            } else {
                rcvParams.matchConstraintMaxHeight = getResources().getDimensionPixelSize(com.sgm.navi.ui.R.dimen.dp_839);
                sllParams.height = getResources().getDimensionPixelSize(com.sgm.navi.ui.R.dimen.dp_713);
            }
            mViewBinding.rcvCollect.setLayoutParams(rcvParams);
        }
    }

    private void hideEmptyView(){
        mViewBinding.sllNoFavorite.setVisibility(GONE);
        mViewBinding.sllNoCharge.setVisibility(GONE);
        mViewBinding.sllNoData.setVisibility(GONE);
        mViewBinding.sllNoNet.setVisibility(GONE);
        mViewBinding.sllNoSgm.setVisibility(GONE);
    }

    BroadcastReceiver mAccountReceiver = new BroadcastReceiver() {
        @Override
        public void onReceive(Context context, Intent intent) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"mAccountReceiver onReceive");
            new SearchConfirmDialog.Build(getContext())
                    .setDialogObserver(new IBaseDialogClickListener() {
                        @Override
                        public void onCommitClick() {
                            mIsAsyncData = true;
                            mViewBinding.naviBroadcastLarge.performClick();
                        }
                        @Override
                        public void onCancelClick() {
                            mIsAsyncData = false;
                            mViewBinding.naviBroadcastLarge.performClick();
                        }
                    })
                    .setContent(getContext().getString(R.string.search_async_sgm_data))
                    .setConfirmTitle(getContext().getString(R.string.search_async))
                    .build().show();
        }
    };

    private void registerBroadcast(){
        final IntentFilter intentFilter = new IntentFilter();
        intentFilter.addAction(PATAC_ACTION_LOGIN);
        ContextCompat.registerReceiver(getContext(), mAccountReceiver, intentFilter, ContextCompat.RECEIVER_EXPORTED);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        getContext().unregisterReceiver(mAccountReceiver);
    }

    public void notifyNetSearchResult(int taskId, ArrayList<PoiInfoEntity> poiInfoEntity){
        if(ConvertUtils.isNull(mScreenViewModel)){
            return;
        }
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"taskId: "+taskId + "--currentTaskId: "+mScreenViewModel.getMTaskId());
        if ((!ConvertUtils.equals(taskId, mScreenViewModel.getMTaskId()) && mScreenViewModel.getMTaskId() != 0) || mViewBinding == null) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"error");
            return;
        }
        setAdapterData(poiInfoEntity,true);
    }

    public void notifySearchResultByNetError(int taskId,String message){
        if(ConvertUtils.isNull(mScreenViewModel)){
            return;
        }
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"error message: " + message + "--taskId: "+taskId + "--currentTaskId: "+mScreenViewModel.getMTaskId());
        if ((!ConvertUtils.equals(taskId, mScreenViewModel.getMTaskId()) && mScreenViewModel.getMTaskId() != 0) || mViewBinding == null) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"error");
            return;
        }
        hideEmptyView();
        mViewBinding.sllNoNet.setVisibility(VISIBLE);
    }

    private AccessTokenParam getAccessTokenParam(Activity activity){
        if(!ConvertUtils.isNull(activity)) return null;
        return new AccessTokenParam(
                AutoMapConstant.AccountTokenParamType.ACCOUNT_TYPE_PATAC_HMI,
                AutoMapConstant.AccountTokenParamType.AUTH_TOKEN_TYPE_READ_ONLY,
                null,
                activity,
                null,
                null,
                null,
                null);
    }
}