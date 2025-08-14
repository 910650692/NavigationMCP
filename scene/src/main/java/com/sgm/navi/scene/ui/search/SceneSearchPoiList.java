package com.sgm.navi.scene.ui.search;


import android.animation.ValueAnimator;
import android.content.Context;
import android.os.Bundle;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.animation.LinearInterpolator;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.constraintlayout.widget.ConstraintLayout;
import androidx.constraintlayout.widget.ConstraintSet;
import androidx.fragment.app.Fragment;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.alibaba.android.arouter.launcher.ARouter;
import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.TimeUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.burypoint.bean.BuryProperty;
import com.sgm.navi.burypoint.constant.BuryConstant;
import com.sgm.navi.burypoint.controller.BuryPointController;
import com.sgm.navi.scene.BaseSceneView;
import com.sgm.navi.scene.R;
import com.sgm.navi.scene.RoutePath;
import com.sgm.navi.scene.adapter.GridSpacingItemDecoration;
import com.sgm.navi.scene.adapter.HorizontalSpaceItemDecoration;
import com.sgm.navi.scene.api.route.ISceneRouteGasStationChargeSelectCallBack;
import com.sgm.navi.scene.api.search.IOnFilterItemClickListener;
import com.sgm.navi.scene.databinding.PoiSearchResultViewBinding;
import com.sgm.navi.scene.impl.search.SceneSearchPoiListImpl;
import com.sgm.navi.scene.impl.search.SearchFragmentFactory;
import com.sgm.navi.scene.ui.adapter.FilterChildListAdapter;
import com.sgm.navi.scene.ui.adapter.FilterListAdapter;
import com.sgm.navi.scene.ui.adapter.QuickFilterListAdapter;
import com.sgm.navi.scene.ui.adapter.SearchResultAdapter;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.layer.refix.LayerPointItemType;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.mapdata.CityDataInfo;
import com.sgm.navi.service.define.mapdata.ProvDataInfo;
import com.sgm.navi.service.define.route.EvRangeOnRouteInfo;
import com.sgm.navi.service.define.route.RouteLineInfo;
import com.sgm.navi.service.define.route.RouteParam;
import com.sgm.navi.service.define.route.RoutePoiType;
import com.sgm.navi.service.define.search.FavoriteInfo;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.search.SearchCategoryLocalInfo;
import com.sgm.navi.service.define.search.SearchChildCategoryLocalInfo;
import com.sgm.navi.service.define.search.SearchGrandChildCategoryLocalInfo;
import com.sgm.navi.service.define.search.SearchResultEntity;
import com.sgm.navi.service.define.utils.BevPowerCarUtils;
import com.sgm.navi.service.logicpaket.mapdata.MapDataPackage;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.service.logicpaket.search.SearchPackage;
import com.sgm.navi.service.logicpaket.setting.SettingUpdateObservable;
import com.sgm.navi.service.logicpaket.user.behavior.BehaviorPackage;
import com.sgm.navi.ui.base.BaseFragment;
import com.sgm.navi.ui.view.SkinConstraintLayout;
import com.sgm.navi.ui.view.SkinTextView;
import com.sgm.navi.ui.view.refresh.RefreshListener;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

public class SceneSearchPoiList extends BaseSceneView<PoiSearchResultViewBinding, SceneSearchPoiListImpl>
        implements ISceneRouteGasStationChargeSelectCallBack {
    private static final String TAG = "SceneSearchPoiList";
    private static final int MAX_LIST_NUMBER = 10;
    private static final String DIVIDER = "_";
    private static final String APPEND = "+";
    private SearchResultAdapter mAdapter;
    private FilterListAdapter mFilterOneAdapter;
    private FilterListAdapter mFilterTwoAdapter;
    private FilterListAdapter mFilterThreeAdapter;
    private FilterListAdapter mFilterFourAdapter;
    private FilterChildListAdapter mFilterOneChildAdapter;
    private FilterChildListAdapter mFilterTwoChildAdapter;
    private FilterChildListAdapter mFilterThreeChildAdapter;
    private FilterChildListAdapter mFilterFourChildAdapter;
    private QuickFilterListAdapter mQuickFilterListAdapter;
    private int maxPageNum = 1;
    private int mPageNum = 1;
    private int mSearchType = AutoMapConstant.SearchType.SEARCH_KEYWORD;
    private String mSearchText;
    private PoiInfoEntity mPoiInfoEntity;
    private SearchResultEntity mResultEntity;
    private List<SearchCategoryLocalInfo> mLocalInfoList;
    private boolean mIsFilterViewShow = false;
    private final int mHorizontalSpacing = 12;
    private final int mQuickLabelHorizontalSpacing = 8;
    private final int mChildHorizontalSpacing = 16;
    private final int mChildVerticalSpacing = 16;
    private final int mSpanCount = 3;
    //第一个一级菜单当前正在被选中的二级菜单下标
    private int mCurrentSelectedIndex1 = -1;
    //第二个一级菜单当前正在被选中的二级菜单下标
    private int mCurrentSelectedIndex2 = -1;
    //第三个一级菜单当前正在被选中的二级菜单下标
    private int mCurrentSelectedIndex3 = -1;
    //第四个一级菜单当前正在被选中的二级菜单下标
    private int mCurrentSelectedIndex4 = -1;
    private int mCurrentSelectedQuick = -1;
    private int mHomeCompanyType;
    private int mRange = 5000;
    private int mCityCode = 0;
    //已下载的城市列表
    private ArrayList<ProvDataInfo> mProvDataInfos;
    private boolean mIsOpenFromNavi;
    private boolean mIsChargeSelf = false;
    private SearchResultEntity mSearchResultEntity;
    private int mTaskId;
    private boolean mIsEnd = false;

    private ViewGroup mSearchContainer;
    private List<SearchChildCategoryLocalInfo> mChildQuickList;
    private String mQuickValue;
    private RoutePackage mRoutePackage;
    private ValueAnimator mAnimator;
    private float mAngelTemp = 0;

    private Runnable mOfflineRunnable = new Runnable() {
        @Override
        public void run() {
            //需要等toast消失候再跳转离线城市页面
            if (mSearchResultEntity != null
                    && mSearchResultEntity.getPoiType() == 0
                    && !ConvertUtils.isEmpty(MapDataPackage.getInstance().getAllDownLoadedList())
                    && mSearchType != AutoMapConstant.SearchType.ALONG_WAY_SEARCH) {
                //离线搜索无数据时，跳转城市列表搜索界面
                final Fragment fragment = (Fragment) ARouter.getInstance()
                        .build(RoutePath.Search.OFFLINE_SEARCH_FRAGMENT)
                        .navigation();
                closeCurrentFragment(false);
                addFragment((BaseFragment) fragment, SearchFragmentFactory.createOfflineFragment(mSearchResultEntity.getKeyword()));
            }
        }
    };

    public SceneSearchPoiList(@NonNull final Context context) {
        super(context);
    }

    public SceneSearchPoiList(@NonNull final Context context, @Nullable final AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneSearchPoiList(@NonNull final Context context, @Nullable final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected PoiSearchResultViewBinding createViewBinding(final LayoutInflater inflater, final ViewGroup viewGroup) {
        return PoiSearchResultViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneSearchPoiListImpl initSceneImpl() {
        return new SceneSearchPoiListImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setIScenePoiList(mScreenViewModel);
    }

    @Override
    protected void initObserver() {
        setupRecyclerView();
        setupSearchActions();
        setupRefreshListener();
        setupFilterActions();
        setupChildListActions();
        mCurrentSelectedQuick = -1;
        mSearchContainer = mViewBinding.searchContainer;
        mRoutePackage = RoutePackage.getInstance();
        initLoadAnim(mViewBinding.ivLoading);
    }


    /**
     * 初始化 RecyclerView
     */
    private void setupRecyclerView() {
        mViewBinding.recyclerSearchResult.setLayoutManager(new LinearLayoutManager(getContext()));
        mAdapter = new SearchResultAdapter();
        mViewBinding.recyclerSearchResult.setAdapter(mAdapter);

        final RecyclerView.ItemDecoration itemDecoration = new HorizontalSpaceItemDecoration(mHorizontalSpacing);
        mViewBinding.searchFilterView.searchFilterList1.setLayoutManager(new LinearLayoutManager(getContext(), RecyclerView.HORIZONTAL, false));
        mFilterOneAdapter = new FilterListAdapter();
        mViewBinding.searchFilterView.searchFilterList1.setAdapter(mFilterOneAdapter);
        mViewBinding.searchFilterView.searchFilterList1.addItemDecoration(itemDecoration);

        mViewBinding.searchFilterView.searchFilterList2.setLayoutManager(new LinearLayoutManager(getContext(), RecyclerView.HORIZONTAL, false));
        mFilterTwoAdapter = new FilterListAdapter();
        mViewBinding.searchFilterView.searchFilterList2.setAdapter(mFilterTwoAdapter);
        mViewBinding.searchFilterView.searchFilterList2.addItemDecoration(itemDecoration);

        mViewBinding.searchFilterView.searchFilterList3.setLayoutManager(new LinearLayoutManager(getContext(), RecyclerView.HORIZONTAL, false));
        mFilterThreeAdapter = new FilterListAdapter();
        mViewBinding.searchFilterView.searchFilterList3.setAdapter(mFilterThreeAdapter);
        mViewBinding.searchFilterView.searchFilterList3.addItemDecoration(itemDecoration);

        mViewBinding.searchFilterView.searchFilterList4.setLayoutManager(new LinearLayoutManager(getContext(), RecyclerView.HORIZONTAL, false));
        mFilterFourAdapter = new FilterListAdapter();
        mViewBinding.searchFilterView.searchFilterList4.setAdapter(mFilterFourAdapter);
        mViewBinding.searchFilterView.searchFilterList4.addItemDecoration(itemDecoration);

        final RecyclerView.ItemDecoration gridDecoration = new GridSpacingItemDecoration(getContext(),
                mSpanCount, mChildVerticalSpacing, mChildHorizontalSpacing, false);
        mViewBinding.searchFilterView.searchFilterList1Child.setLayoutManager(new GridLayoutManager(getContext(), mSpanCount));
        mViewBinding.searchFilterView.searchFilterList1Child.setNestedScrollingEnabled(false);
        mFilterOneChildAdapter = new FilterChildListAdapter();
        mViewBinding.searchFilterView.searchFilterList1Child.setAdapter(mFilterOneChildAdapter);
        mViewBinding.searchFilterView.searchFilterList1Child.addItemDecoration(gridDecoration);

        mViewBinding.searchFilterView.searchFilterList2Child.setLayoutManager(new GridLayoutManager(getContext(), mSpanCount));
        mViewBinding.searchFilterView.searchFilterList2Child.setNestedScrollingEnabled(false);
        mFilterTwoChildAdapter = new FilterChildListAdapter();
        mViewBinding.searchFilterView.searchFilterList2Child.setAdapter(mFilterTwoChildAdapter);
        mViewBinding.searchFilterView.searchFilterList2Child.addItemDecoration(gridDecoration);

        mViewBinding.searchFilterView.searchFilterList3Child.setLayoutManager(new GridLayoutManager(getContext(), mSpanCount));
        mViewBinding.searchFilterView.searchFilterList3Child.setNestedScrollingEnabled(false);
        mFilterThreeChildAdapter = new FilterChildListAdapter();
        mViewBinding.searchFilterView.searchFilterList3Child.setAdapter(mFilterThreeChildAdapter);
        mViewBinding.searchFilterView.searchFilterList3Child.addItemDecoration(gridDecoration);

        mViewBinding.searchFilterView.searchFilterList4Child.setLayoutManager(new GridLayoutManager(getContext(), mSpanCount));
        mViewBinding.searchFilterView.searchFilterList4Child.setNestedScrollingEnabled(false);
        mFilterFourChildAdapter = new FilterChildListAdapter();
        mViewBinding.searchFilterView.searchFilterList4Child.setAdapter(mFilterFourChildAdapter);
        mViewBinding.searchFilterView.searchFilterList4Child.addItemDecoration(gridDecoration);

        // 快筛adapter
        final RecyclerView.ItemDecoration quickItemDecoration = new HorizontalSpaceItemDecoration(mQuickLabelHorizontalSpacing);
        mViewBinding.searchLabelFilter.setLayoutManager(new LinearLayoutManager(getContext(), RecyclerView.HORIZONTAL, false));
        mQuickFilterListAdapter = new QuickFilterListAdapter();
        mViewBinding.searchLabelFilter.setAdapter(mQuickFilterListAdapter);
        mViewBinding.searchLabelFilter.addItemDecoration(quickItemDecoration);

        mViewBinding.routeChargeListAlongWayCancel.setOnClickListener(new OnClickListener() {
            @Override
            public void onClick(View view) {
                mScreenViewModel.closeSearch();
            }
        });

        mViewBinding.routeChargeListAlongWaySure.setOnClickListener(new OnClickListener() {
            @Override
            public void onClick(View view) {
                startAllRequest();
            }
        });

        mAdapter.setOnItemClickListener(new SearchResultAdapter.OnItemClickListener() {
            @Override
            public void onItemClick(final int position, final PoiInfoEntity poiInfoEntity) {
                sendBuryPointForListSelect(position+1, poiInfoEntity.getName());
                final Fragment fragment = (Fragment) ARouter.getInstance().build(RoutePath.Search.POI_DETAILS_FRAGMENT).navigation();
                if (!ConvertUtils.isEmpty(mScreenViewModel) && !ConvertUtils.isEmpty(mResultEntity)) {
                    mScreenViewModel.setSelectIndex(poiInfoEntity, position, mSearchType);
                    final List<PoiInfoEntity> poiInfoEntities = mResultEntity.getPoiList();
                    if (!ConvertUtils.isEmpty(poiInfoEntities)) {
                        // 遍历所有可见的item
                        for (int i = 0; i < poiInfoEntities.size(); i++) {
                            if (!ConvertUtils.isEmpty(poiInfoEntities)) {
//                                if (ConvertUtils.equals(poiInfoEntities.get(i).getPid(), poiInfoEntity.getPid())) {
                                poiInfoEntities.get(i).setMIsVisible(false);
//                                }
                            }
                        }
                        mScreenViewModel.updatePoiMarker(poiInfoEntities, 0, true);
                    }
                }
                final int poiType = getPoiType(mAdapter.getHomeCompanyType());
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onClick poiType: " , poiType , " homeCompany: " , mAdapter.getHomeCompanyType());
                final Bundle bundle = SearchFragmentFactory.createPoiDetailsFragment(
                        AutoMapConstant.SourceFragment.SEARCH_RESULT_FRAGMENT, poiType, poiInfoEntity);
                bundle.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_SOURCE_DATA, mResultEntity);
                bundle.putBoolean("IS_END", mIsEnd);
                if(!ConvertUtils.isEmpty(mChildQuickList) && mCurrentSelectedQuick > 0){
                    bundle.putString("LABEL",mChildQuickList.get(mCurrentSelectedQuick).getName());
                }
                addPoiDetailsFragment((BaseFragment) fragment, bundle);
            }

            @Override
            public void onChildItemClick(final int position, final PoiInfoEntity poiInfoEntity, final int childPosition) {
                sendBuryPointForListSelect(position+1, poiInfoEntity.getName());
                final Fragment fragment = (Fragment) ARouter.getInstance().build(RoutePath.Search.POI_DETAILS_FRAGMENT).navigation();
                if (!ConvertUtils.isEmpty(mScreenViewModel) && !ConvertUtils.isEmpty(mResultEntity)) {
                    mScreenViewModel.setSelectIndex(poiInfoEntity, position, mSearchType);
                }
                final int poiType = getPoiType(mAdapter.getHomeCompanyType());
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onClick poiType: " + poiType + " homeCompany: " + mAdapter.getHomeCompanyType());
                final Bundle bundle = SearchFragmentFactory.createPoiDetailsFragment(
                        AutoMapConstant.SourceFragment.SEARCH_RESULT_FRAGMENT, poiType, poiInfoEntity);
                bundle.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_SOURCE_DATA, mResultEntity);
                bundle.putInt(AutoMapConstant.ChildIndex.BUNDLE_CHILD_INDEX, childPosition);
                bundle.putBoolean("IS_END", mIsEnd);
                addPoiDetailsFragment((BaseFragment) fragment, bundle);
            }

            @Override
            public void onNaviClick(final int position, final PoiInfoEntity poiInfoEntity) {
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onNaviClick: " + mAdapter.getHomeCompanyType());
                if (mAdapter.getHomeCompanyType() == 1
                        || mAdapter.getHomeCompanyType() == 2
                        || mAdapter.getHomeCompanyType() == 3
                        || mAdapter.getHomeCompanyType() == 0) {
                    ToastUtils.Companion.getInstance().showCustomToastView(
                            ResourceUtils.Companion.getInstance().getString(R.string.smp_set_success), 0);
                    final int commonName = mAdapter.getHomeCompanyType();
                    final FavoriteInfo favoriteInfo = new FavoriteInfo();
                    favoriteInfo.setCommonName(commonName)
                            .setUpdateTime(new Date().getTime());
                    poiInfoEntity.setFavoriteInfo(favoriteInfo);
                    BehaviorPackage.getInstance().addFavorite(poiInfoEntity, commonName);
//                    BehaviorPackage.getInstance().addFavoriteData(poiInfoEntity, commonName);
                    SettingUpdateObservable.getInstance().onUpdateSyncTime();
                    closeAllFragmentsUntilTargetFragment("HomeCompanyFragment");
                    showCurrentFragment();
                    mScreenViewModel.clearLabelMarker();
                } else {
                    if (SearchPackage.getInstance().isAlongWaySearch() && !mIsEnd) {
                        addRemoveClick(position, poiInfoEntity);
                    } else {
                        if (mIsEnd) {
                            ThreadManager.getInstance().execute(() -> mRoutePackage.requestChangeEnd(mMapTypeId, poiInfoEntity));
                        } else {
                            SearchPackage.getInstance().clearLabelMark();
                            final Fragment fragment = (Fragment) ARouter.getInstance()
                                    .build(RoutePath.Route.ROUTE_FRAGMENT)
                                    .navigation();
                            addFragment((BaseFragment) fragment, SearchFragmentFactory.createRouteFragment(poiInfoEntity));
                        }
                    }
                }
            }
        });

        mViewBinding.recyclerSearchResult.addOnScrollListener(new RecyclerView.OnScrollListener() {
            @Override
            public void onScrollStateChanged(@NonNull final RecyclerView recyclerView, final int newState) {
                super.onScrollStateChanged(recyclerView, newState);
                if (newState == RecyclerView.SCROLL_STATE_IDLE && mSearchType != AutoMapConstant.SearchType.EN_ROUTE_KEYWORD_SEARCH) {
                    updatePoiMarkerVisibleState();
                }
            }
        });
    }

    /**
     * 更新列表后，更新列表item的选中状态
     */
    private void updatePoiMarkerVisibleState() {
        //滑动结束后，高亮对应item
        final RecyclerView.LayoutManager layoutManager = mViewBinding.recyclerSearchResult.getLayoutManager();
        final LinearLayoutManager linearLayoutManager = (LinearLayoutManager) layoutManager;
        mViewBinding.recyclerSearchResult.post(new Runnable() {
            @Override
            public void run() {
                // 获取第一个可见item的位置
                if (linearLayoutManager != null) {
                    final int firstVisiblePosition = linearLayoutManager.findFirstVisibleItemPosition();
                    // 获取最后一个可见item的位置
                    final int lastVisiblePosition = linearLayoutManager.findLastVisibleItemPosition();

                    Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "第一个可见位置: " + firstVisiblePosition);
                    Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "最后一个可见位置: " + lastVisiblePosition);
                    if (mAdapter == null || mResultEntity == null) {
                        return;
                    }
                    final List<PoiInfoEntity> poiInfoEntities = mResultEntity.getPoiList();
//                    if (firstVisiblePosition < 0 || firstVisiblePosition >= mAdapter.getItemCount()
//                            || lastVisiblePosition < 0 || lastVisiblePosition >= mAdapter.getItemCount()
//                            || firstVisiblePosition >= lastVisiblePosition) {
//                        if (!ConvertUtils.isEmpty(poiInfoEntities)) {
//                            //首次进入可见位置均是-1，默认选中下标0-2的item
//                            final int size = Math.min(3, poiInfoEntities.size());
//                            for (int i = 0; i < poiInfoEntities.size(); i++) {
//                                final PoiInfoEntity poiInfoEntity = poiInfoEntities.get(i);
//                                poiInfoEntity.setMIsVisible(i <= size - 1);
//                            }
//                            mScreenViewModel.updatePoiMarker(poiInfoEntities, 0, true);
//                        }
//                        return;
//                    }
                    if (!ConvertUtils.isEmpty(poiInfoEntities)) {
                        // 遍历所有可见的item
                        for (int i = 0; i < poiInfoEntities.size(); i++) {
                            if (!ConvertUtils.isEmpty(poiInfoEntities) && !mScreenViewModel.isAlongWaySearch()) {
                                final PoiInfoEntity poiInfoEntity = poiInfoEntities.get(i);
                                poiInfoEntity.setMIsVisible(i >= firstVisiblePosition && i <= lastVisiblePosition);
                            }
                        }
                        mScreenViewModel.updatePoiMarker(poiInfoEntities, 0, true);
                    }
                }
            }
        });

    }

    /**
     * 获取poiType
     *
     * @param type 跳转类型
     * @return AutoMapConstant.PoiType
     */
    private int getPoiType(final int type) {
        int poiType = AutoMapConstant.PoiType.POI_KEYWORD;
        // 1:家 2:公司 3:常用地址 0:收藏夹 -1:都不是
        if (type == 1) {
            poiType = AutoMapConstant.PoiType.POI_HOME;
        } else if (type == 2) {
            poiType = AutoMapConstant.PoiType.POI_COMPANY;
        } else if (type == 3) {
            poiType = AutoMapConstant.PoiType.POI_COMMON;
        } else if (type == 0) {
            poiType = AutoMapConstant.PoiType.POI_COLLECTION;
        }
        return poiType;
    }

    /**
     * 配置搜索相关事件
     */
    private void setupSearchActions() {
        mViewBinding.searchTextBarView.ivClose.setOnClickListener(view -> {
            // 筛选页退出不直接返回上一级
            if(mIsFilterViewShow){
                hideFilterPage();
                return;
            }
            mScreenViewModel.abortSearchByTaskId();
            if (mIsOpenFromNavi) {
                mScreenViewModel.closeSearchOpenFromNavi();
            } else {
                mScreenViewModel.closeSearch();
            }
        });
        mViewBinding.searchTextBarView.searchBarTextView.setOnClickListener(v -> {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "setupSearchActions: " + mSearchType);
            if (mSearchType == AutoMapConstant.SearchType.SEARCH_KEYWORD) {
                // 筛选页退出不直接返回上一级
                if(mIsFilterViewShow){
                    return;
                }
                if (mIsOpenFromNavi) {
                    mScreenViewModel.closeSearchOpenFromNavi();
                } else {
                    mScreenViewModel.closeSearch();
                }
            }

        });
    }

    /**
     * 设置筛选页面相关配置
     */
    private void setupFilterNormalActions() {
        mViewBinding.searchFilterView.searchFilterRoot.setVisibility(GONE);
        updateChargeList();
        mViewBinding.searchFilterView.searchFilterConfirm.setOnClickListener(v -> {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "click confirm: ");
            mCurrentSelectedQuick = -1;
            mViewBinding.searchFilterView.searchFilterRoot.setVisibility(GONE);
            updateChargeList();
            mViewBinding.pullRefreshLayout.setVisibility(VISIBLE);
            mIsFilterViewShow = false;
            updateChargeList();
            mViewBinding.searchTextBarView.searchBarTextView.setText(mSearchText);
            if (mSearchType == AutoMapConstant.SearchType.AROUND_SEARCH) {
                mScreenViewModel.aroundSearch(mPageNum, mSearchText, mResultEntity.getRetain(), getClassifyData(), false, mPoiInfoEntity);
            } else if(mSearchType == AutoMapConstant.SearchType.EN_ROUTE_KEYWORD_SEARCH){
                mScreenViewModel.alongWaySearch(mSearchText, mResultEntity.getRetain(), getClassifyData(), false);
            } else {
                mScreenViewModel.keywordSearch(mPageNum, mSearchText, mResultEntity.getRetain(), getClassifyData(), false);
            }
            if (!ConvertUtils.isNull(mAnimator)) {
                if(mAnimator.isRunning()){
                    Logger.e(MapDefaultFinalTag.SEARCH_HMI_TAG, "mSearchLoadingDialog is showing");
                }else{
                    showLoading(true);
                }
            }
        });
        mViewBinding.searchFilterView.searchFilterCancel.setOnClickListener(v -> {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "click reset: ");
            mViewBinding.searchFilterView.searchFilterRoot.setVisibility(GONE);
            updateChargeList();
            mViewBinding.pullRefreshLayout.setVisibility(VISIBLE);
            mIsFilterViewShow = false;
            updateChargeList();
            mViewBinding.searchTextBarView.searchBarTextView.setText(mSearchText);
            if (mSearchType == AutoMapConstant.SearchType.AROUND_SEARCH) {
                mScreenViewModel.aroundSearch(mPageNum, mSearchText, mPoiInfoEntity, String.valueOf(mRange), false);
            } else if(mSearchType == AutoMapConstant.SearchType.EN_ROUTE_KEYWORD_SEARCH){
                mScreenViewModel.alongWaySearch(mSearchText);
            } else {
                mScreenViewModel.keywordSearch(mPageNum, mSearchText);
            }
            if (!ConvertUtils.isNull(mAnimator)) {
                if(mAnimator.isRunning()){
                    Logger.e(MapDefaultFinalTag.SEARCH_HMI_TAG, "mSearchLoadingDialog is showing");
                }else{
                    showLoading(true);
                }
            }
        });
        mViewBinding.searchTextBarView.csFilter.setOnClickListener(v -> {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "click filter: ");
            if (mAnimator != null && mAnimator.isRunning()) {
                return;
            }
            if(mIsChargeSelf){
                ToastUtils.Companion.getInstance().showCustomToastView(getContext().getString(R.string.search_charge_self_filter));
                return;
            }
            if (!mIsFilterViewShow) {
                mViewBinding.searchFilterView.searchFilterRoot.setVisibility(VISIBLE);
                mViewBinding.routeChargeListAlongWaySure.setVisibility(View.GONE);
                mViewBinding.routeChargeListAlongWayCancel.setVisibility(View.GONE);
                mViewBinding.pullRefreshLayout.setVisibility(GONE);
                mViewBinding.searchResultNoData.setVisibility(GONE);
                mViewBinding.searchLabelFilter.setVisibility(GONE);
                mIsFilterViewShow = true;
                if (null != mResultEntity) {
                    mViewBinding.searchTextBarView.searchBarTextView.setText(getContext().getString(
                            R.string.filter_result, mSearchText, mResultEntity.getTotal()));
                }
            }
            if (!ConvertUtils.isEmpty(mLocalInfoList)) {
                for (int i = 0; i < mLocalInfoList.size(); i++) {
                    final SearchCategoryLocalInfo searchCategoryLocalInfo = mLocalInfoList.get(i);
                    if (i == 0) {
                        if (ConvertUtils.isEmpty(searchCategoryLocalInfo.getCategoryLocalInfos())) {
                            mViewBinding.searchFilterView.searchFilterTitle1.setVisibility(GONE);
                            mViewBinding.searchFilterView.searchFilterList1.setVisibility(GONE);
                            mViewBinding.searchFilterView.searchFilterList1Child.setVisibility(GONE);
                        }else{
                            mViewBinding.searchFilterView.searchFilterTitle1.setVisibility(VISIBLE);
                            mViewBinding.searchFilterView.searchFilterList1.setVisibility(VISIBLE);
                            mViewBinding.searchFilterView.searchFilterList1Child.setVisibility(VISIBLE);
                        }
                        mViewBinding.searchFilterView.searchFilterTitle1.setText(searchCategoryLocalInfo.getName());
                        mFilterOneAdapter.setMCurrentExpandName("");
                        mFilterOneAdapter.setCategoryList(searchCategoryLocalInfo.getCategoryLocalInfos());
                        if(!ConvertUtils.isNull(searchCategoryLocalInfo.getCategoryLocalInfos()) && searchCategoryLocalInfo.getCategoryLocalInfos().size() == 1){
                            mViewBinding.searchFilterView.searchFilterList1.setVisibility(GONE);
                            mFilterOneAdapter.setMIsExpand(true);
                            mFilterThreeChildAdapter.setCollapse(true);
                            mFilterOneChildAdapter.setCategoryList(searchCategoryLocalInfo.getCategoryLocalInfos().get(0).getCategoryLocalInfos());
                        }else{
                            mFilterOneAdapter.setMIsExpand(false);
                            mFilterOneChildAdapter.setCategoryList(null);
                        }

                    } else if (i == 1) {
                        if (ConvertUtils.isEmpty(searchCategoryLocalInfo.getCategoryLocalInfos())) {
                            mViewBinding.searchFilterView.searchFilterTitle2.setVisibility(GONE);
                            mViewBinding.searchFilterView.searchFilterList2.setVisibility(GONE);
                            mViewBinding.searchFilterView.searchFilterList2Child.setVisibility(GONE);
                        }else{
                            mViewBinding.searchFilterView.searchFilterTitle2.setVisibility(VISIBLE);
                            mViewBinding.searchFilterView.searchFilterList2.setVisibility(VISIBLE);
                            mViewBinding.searchFilterView.searchFilterList2Child.setVisibility(VISIBLE);
                        }
                        mViewBinding.searchFilterView.searchFilterTitle2.setText(searchCategoryLocalInfo.getName());
                        mFilterTwoAdapter.setMCurrentExpandName("");
                        mFilterTwoAdapter.setCategoryList(searchCategoryLocalInfo.getCategoryLocalInfos());
                        if(!ConvertUtils.isNull(searchCategoryLocalInfo.getCategoryLocalInfos()) && searchCategoryLocalInfo.getCategoryLocalInfos().size() == 1){
                            mViewBinding.searchFilterView.searchFilterList2.setVisibility(GONE);
                            mFilterTwoAdapter.setMIsExpand(true);
                            mFilterThreeChildAdapter.setCollapse(true);
                            mFilterTwoChildAdapter.setCategoryList(searchCategoryLocalInfo.getCategoryLocalInfos().get(0).getCategoryLocalInfos());
                        }else{
                            mFilterTwoAdapter.setMIsExpand(false);
                            mFilterTwoChildAdapter.setCategoryList(null);
                        }
                    } else if (i == 2) {
                        if (ConvertUtils.isEmpty(searchCategoryLocalInfo.getCategoryLocalInfos())) {
                            mViewBinding.searchFilterView.searchFilterTitle3.setVisibility(GONE);
                            mViewBinding.searchFilterView.searchFilterList3.setVisibility(GONE);
                            mViewBinding.searchFilterView.searchFilterList3Child.setVisibility(GONE);
                        }else{
                            mViewBinding.searchFilterView.searchFilterTitle3.setVisibility(VISIBLE);
                            mViewBinding.searchFilterView.searchFilterList3.setVisibility(VISIBLE);
                            mViewBinding.searchFilterView.searchFilterList3Child.setVisibility(VISIBLE);
                        }
                        mViewBinding.searchFilterView.searchFilterTitle3.setText(searchCategoryLocalInfo.getName());
                        mFilterThreeAdapter.setMCurrentExpandName("");
                        mFilterThreeAdapter.setCategoryList(searchCategoryLocalInfo.getCategoryLocalInfos());
                        if(!ConvertUtils.isNull(searchCategoryLocalInfo.getCategoryLocalInfos()) && searchCategoryLocalInfo.getCategoryLocalInfos().size() == 1){
                            mViewBinding.searchFilterView.searchFilterList3.setVisibility(GONE);
                            mFilterThreeAdapter.setMIsExpand(true);
                            mFilterThreeChildAdapter.setCollapse(true);
                            mFilterThreeChildAdapter.setCharge(!ConvertUtils.isEmpty(getEditText()) && getEditText().startsWith("充电站"));
                            mFilterThreeChildAdapter.setCategoryList(searchCategoryLocalInfo.getCategoryLocalInfos().get(0).getCategoryLocalInfos());
                        }else{
                            mFilterThreeAdapter.setMIsExpand(false);
                            mFilterThreeChildAdapter.setCategoryList(null);
                        }
                    } else if (i == 3) {
                        if (ConvertUtils.isEmpty(searchCategoryLocalInfo.getCategoryLocalInfos())) {
                            mViewBinding.searchFilterView.searchFilterTitle4.setVisibility(GONE);
                            mViewBinding.searchFilterView.searchFilterList4.setVisibility(GONE);
                            mViewBinding.searchFilterView.searchFilterList4Child.setVisibility(GONE);
                        }else{
                            mViewBinding.searchFilterView.searchFilterTitle4.setVisibility(VISIBLE);
                            mViewBinding.searchFilterView.searchFilterList4.setVisibility(VISIBLE);
                            mViewBinding.searchFilterView.searchFilterList4Child.setVisibility(VISIBLE);
                        }
                        mViewBinding.searchFilterView.searchFilterTitle4.setText(searchCategoryLocalInfo.getName());
                        mFilterFourAdapter.setMCurrentExpandName("");
                        mFilterFourAdapter.setCategoryList(searchCategoryLocalInfo.getCategoryLocalInfos());
                        if(!ConvertUtils.isNull(searchCategoryLocalInfo.getCategoryLocalInfos()) && searchCategoryLocalInfo.getCategoryLocalInfos().size() == 1){
                            mViewBinding.searchFilterView.searchFilterList4.setVisibility(GONE);
                            mFilterFourAdapter.setMIsExpand(true);
                            mFilterThreeChildAdapter.setCollapse(true);
                            mFilterFourChildAdapter.setCategoryList(searchCategoryLocalInfo.getCategoryLocalInfos().get(0).getCategoryLocalInfos());
                        }else{
                            mFilterFourAdapter.setMIsExpand(false);
                            mFilterFourChildAdapter.setCategoryList(null);
                        }
                    }
                }
            }
        });
    }

    /**
     * 设置筛选页面列表item相关配置
     */
    private void setupFilterActions() {
        setupFilterNormalActions();
        mFilterOneAdapter.setFilterItemClickListener(new IOnFilterItemClickListener() {
            @Override
            public void onItemClick(final int position) {
                mCurrentSelectedIndex1 = position;
                refreshLocalInfoListCheckedState(0, mCurrentSelectedIndex1);
                mFilterOneChildAdapter.setCategoryList(null);
                if (mSearchType == AutoMapConstant.SearchType.AROUND_SEARCH) {
                    mScreenViewModel.aroundSearch(mPageNum, mSearchText, mResultEntity.getRetain(), getClassifyData(), true, mPoiInfoEntity);
                } else if(mSearchType == AutoMapConstant.SearchType.EN_ROUTE_KEYWORD_SEARCH){
                    mScreenViewModel.alongWaySearch(mSearchText, mResultEntity.getRetain(), getClassifyData(), true);
                } else {
                    mScreenViewModel.keywordSearch(mPageNum, mSearchText, mResultEntity.getRetain(), getClassifyData(), true);
                }
            }

            @Override
            public void onChildListExpandCollapse(final List<SearchGrandChildCategoryLocalInfo> childList, final int position) {
                mCurrentSelectedIndex1 = position;
                refreshLocalInfoListCheckedState(0, mCurrentSelectedIndex1);
                mFilterOneChildAdapter.setCategoryList(childList);
//                mViewBinding.searchFilterView.searchFilterScrollview.post(()
//                        -> {
//                    if (mViewBinding != null) {
//                        mViewBinding.searchFilterView.searchFilterScrollview.smoothScrollTo(0,
//                                mViewBinding.searchFilterView.searchFilterList1Child.getBottom());
//                    }
//                });
            }
        });
        mFilterTwoAdapter.setFilterItemClickListener(new IOnFilterItemClickListener() {
            @Override
            public void onItemClick(final int position) {
                mCurrentSelectedIndex2 = position;
                refreshLocalInfoListCheckedState(1, mCurrentSelectedIndex2);
                mFilterTwoChildAdapter.setCategoryList(null);
                if (mSearchType == AutoMapConstant.SearchType.AROUND_SEARCH) {
                    mScreenViewModel.aroundSearch(mPageNum, mSearchText, mResultEntity.getRetain(), getClassifyData(), true, mPoiInfoEntity);
                } else if(mSearchType == AutoMapConstant.SearchType.EN_ROUTE_KEYWORD_SEARCH){
                    mScreenViewModel.alongWaySearch(mSearchText, mResultEntity.getRetain(), getClassifyData(), true);
                } else {
                    mScreenViewModel.keywordSearch(mPageNum, mSearchText, mResultEntity.getRetain(), getClassifyData(), true);
                }
            }

            @Override
            public void onChildListExpandCollapse(final List<SearchGrandChildCategoryLocalInfo> childList, final int position) {
                mCurrentSelectedIndex2 = position;
                refreshLocalInfoListCheckedState(1, mCurrentSelectedIndex2);
                mFilterTwoChildAdapter.setCategoryList(childList);
//                mViewBinding.searchFilterView.searchFilterScrollview.post(()
//                        -> {
//                    if (mViewBinding != null) {
//                        mViewBinding.searchFilterView.searchFilterScrollview.smoothScrollTo(0,
//                                mViewBinding.searchFilterView.searchFilterList2Child.getBottom());
//                    }
//                });
            }
        });
        mFilterThreeAdapter.setFilterItemClickListener(new IOnFilterItemClickListener() {
            @Override
            public void onItemClick(final int position) {
                mCurrentSelectedIndex3 = position;
                refreshLocalInfoListCheckedState(2, mCurrentSelectedIndex3);
                mFilterThreeChildAdapter.setCategoryList(null);
                if (mSearchType == AutoMapConstant.SearchType.AROUND_SEARCH) {
                    mScreenViewModel.aroundSearch(mPageNum, mSearchText, mResultEntity.getRetain(), getClassifyData(), true, mPoiInfoEntity);
                } else if(mSearchType == AutoMapConstant.SearchType.EN_ROUTE_KEYWORD_SEARCH){
                    mScreenViewModel.alongWaySearch(mSearchText, mResultEntity.getRetain(), getClassifyData(), true);
                } else {
                    mScreenViewModel.keywordSearch(mPageNum, mSearchText, mResultEntity.getRetain(), getClassifyData(), true);
                }
            }

            @Override
            public void onChildListExpandCollapse(final List<SearchGrandChildCategoryLocalInfo> childList, final int position) {
                mCurrentSelectedIndex3 = position;
                refreshLocalInfoListCheckedState(2, mCurrentSelectedIndex3);
                mFilterThreeChildAdapter.setCharge(!ConvertUtils.isEmpty(getEditText()) && getEditText().startsWith("充电站"));
                mFilterThreeChildAdapter.setCategoryList(childList);
//                mViewBinding.searchFilterView.searchFilterScrollview.post(()
//                        -> {
//                    if (mViewBinding != null) {
//                        mViewBinding.searchFilterView.searchFilterScrollview.smoothScrollTo(0,
//                                mViewBinding.searchFilterView.searchFilterList3Child.getBottom());
//                    }
//                });
            }
        });

        mFilterFourAdapter.setFilterItemClickListener(new IOnFilterItemClickListener() {
            @Override
            public void onItemClick(final int position) {
                mCurrentSelectedIndex4 = position;
                refreshLocalInfoListCheckedState(3, mCurrentSelectedIndex4);
                mFilterFourChildAdapter.setCategoryList(null);
                if (mSearchType == AutoMapConstant.SearchType.AROUND_SEARCH) {
                    mScreenViewModel.aroundSearch(mPageNum, mSearchText, mResultEntity.getRetain(), getClassifyData(), true, mPoiInfoEntity);
                } else if(mSearchType == AutoMapConstant.SearchType.EN_ROUTE_KEYWORD_SEARCH){
                    mScreenViewModel.alongWaySearch(mSearchText, mResultEntity.getRetain(), getClassifyData(), true);
                } else {
                    mScreenViewModel.keywordSearch(mPageNum, mSearchText, mResultEntity.getRetain(), getClassifyData(), true);
                }
            }

            @Override
            public void onChildListExpandCollapse(final List<SearchGrandChildCategoryLocalInfo> childList, final int position) {
                mCurrentSelectedIndex4 = position;
                refreshLocalInfoListCheckedState(3, mCurrentSelectedIndex4);
                mFilterFourChildAdapter.setCategoryList(childList);
//                mViewBinding.searchFilterView.searchFilterScrollview.post(()
//                        -> {
//                    if (mViewBinding != null) {
//                        mViewBinding.searchFilterView.searchFilterScrollview.smoothScrollTo(0,
//                                mViewBinding.searchFilterView.searchFilterList4Child.getBottom());
//                    }
//                });
            }
        });

        mQuickFilterListAdapter.setItemClickListener(new QuickFilterListAdapter.OnItemClickListener() {
            @Override
            public void onItemClick(List<SearchChildCategoryLocalInfo> list,int position) {
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"value: "+list + "position: "+position);
                if(position>list.size()) return;
                if (!ConvertUtils.isNull(mAnimator)) {
                    if(mAnimator.isRunning()){
                        Logger.e(MapDefaultFinalTag.SEARCH_HMI_TAG, "mSearchLoadingDialog is showing");
                    }else{
                        showLoading(true);
                    }
                }
                mPageNum = 1;
                if(mCurrentSelectedQuick == position){
                    mCurrentSelectedQuick = -1;
                    if(!ConvertUtils.isNull(mAdapter)){
                        mAdapter.setQuickLabel(new ArrayList<>());
                    }
                    mQuickValue = "";
                    if(mSearchType == AutoMapConstant.SearchType.AROUND_SEARCH){
                        mScreenViewModel.aroundSearch(mPageNum,mSearchText,"","",false,mPoiInfoEntity);
                    }else {
                        mScreenViewModel.keywordSearch(mPageNum,mSearchText);
                    }

                    return;
                }
                mCurrentSelectedQuick = position;
                SearchChildCategoryLocalInfo info = list.get(position);
                if("charge".equals(info.getValue())){
                    mAdapter.clearList();
                    // 请求SGM自营站数据
                    mScreenViewModel.getAppKey(mSearchResultEntity);
//                    mSearchLoadingDialog.dismiss();
//                    ToastUtils.Companion.getInstance().showCustomToastView(getContext().getString(R.string.search_charge_self_filter_hint));
                }else{
                    Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"value: "+info.getValue());
                    String retain = "";
                    if(!ConvertUtils.isNull(mResultEntity)){
                        retain = mResultEntity.getRetain();
                    }
                    mQuickValue = info.getValue();
                    if(info.getValue().contains("_")){
                        if("searchlist_charging_mode_fast".equals(info.getValue())){
                            refreshLocalInfoListCheckedState(1, 2);
                        }
                        if(mSearchType == AutoMapConstant.SearchType.AROUND_SEARCH){
                            mScreenViewModel.aroundSearch(mPageNum,mSearchText,retain,info.getValue(),false,mPoiInfoEntity);
                        }else{
                            mScreenViewModel.keywordSearch(mPageNum,mSearchText,retain,info.getValue(),false);
                        }
                    }else{
                        if(mSearchType == AutoMapConstant.SearchType.AROUND_SEARCH){
                            mScreenViewModel.aroundSearchByQuickFilter(mPageNum,mSearchText,retain,info.getValue(),false,mPoiInfoEntity);
                        }else{
                            mScreenViewModel.keywordSearchByQuickFilter(mPageNum,mSearchText,retain,info.getValue(),false);
                        }
                    }
                }
            }
        });
    }

    /**
     * 设置子列表点击事件
     */
    private void setupChildListActions() {
        mFilterOneChildAdapter.setFilterItemClickListener(position -> {
            refreshLocalInfoListCheckedState(0, mCurrentSelectedIndex1);
            mFilterOneAdapter.notifyDataSetChanged();
            if (mSearchType == AutoMapConstant.SearchType.AROUND_SEARCH) {
                mScreenViewModel.aroundSearch(mPageNum, mSearchText, mResultEntity.getRetain(), getClassifyData(), true, mPoiInfoEntity);
            } else if(mSearchType == AutoMapConstant.SearchType.EN_ROUTE_KEYWORD_SEARCH){
                mScreenViewModel.alongWaySearch(mSearchText, mResultEntity.getRetain(), getClassifyData(), true);
            } else {
                mScreenViewModel.keywordSearch(mPageNum, mSearchText, mResultEntity.getRetain(), getClassifyData(), true);
            }
        });

        mFilterTwoChildAdapter.setFilterItemClickListener(position -> {
            refreshLocalInfoListCheckedState(1, mCurrentSelectedIndex2);
            mFilterOneAdapter.notifyDataSetChanged();
            if (mSearchType == AutoMapConstant.SearchType.AROUND_SEARCH) {
                mScreenViewModel.aroundSearch(mPageNum, mSearchText, mResultEntity.getRetain(), getClassifyData(), true, mPoiInfoEntity);
            } else if(mSearchType == AutoMapConstant.SearchType.EN_ROUTE_KEYWORD_SEARCH){
                mScreenViewModel.alongWaySearch(mSearchText, mResultEntity.getRetain(), getClassifyData(), true);
            } else {
                mScreenViewModel.keywordSearch(mPageNum, mSearchText, mResultEntity.getRetain(), getClassifyData(), true);
            }
        });

        mFilterThreeChildAdapter.setFilterItemClickListener(position -> {
            refreshLocalInfoListCheckedState(2, mCurrentSelectedIndex3);
            mFilterOneAdapter.notifyDataSetChanged();
            if (mSearchType == AutoMapConstant.SearchType.AROUND_SEARCH) {
                mScreenViewModel.aroundSearch(mPageNum, mSearchText, mResultEntity.getRetain(), getClassifyData(), true, mPoiInfoEntity);
            } else if(mSearchType == AutoMapConstant.SearchType.EN_ROUTE_KEYWORD_SEARCH){
                mScreenViewModel.alongWaySearch(mSearchText, mResultEntity.getRetain(), getClassifyData(), true);
            } else {
                mScreenViewModel.keywordSearch(mPageNum, mSearchText, mResultEntity.getRetain(), getClassifyData(), true);
            }
        });

        mFilterFourChildAdapter.setFilterItemClickListener(position -> {
            refreshLocalInfoListCheckedState(2, mCurrentSelectedIndex4);
            mFilterOneAdapter.notifyDataSetChanged();
            if (mSearchType == AutoMapConstant.SearchType.AROUND_SEARCH) {
                mScreenViewModel.aroundSearch(mPageNum, mSearchText, mResultEntity.getRetain(), getClassifyData(), true, mPoiInfoEntity);
            } else if(mSearchType == AutoMapConstant.SearchType.EN_ROUTE_KEYWORD_SEARCH){
                mScreenViewModel.alongWaySearch(mSearchText, mResultEntity.getRetain(), getClassifyData(), true);
            } else {
                mScreenViewModel.keywordSearch(mPageNum, mSearchText, mResultEntity.getRetain(), getClassifyData(), true);
            }
        });
    }

    /**
     * 刷新本地信息列表选中状态
     *
     * @param index1 二级列表选中下标
     * @param index2 三级列表选中下标
     */
    private void refreshLocalInfoListCheckedState(final int index1, final int index2) {
        if (mLocalInfoList != null
                && !mLocalInfoList.isEmpty()
                && index1 < mLocalInfoList.size() - 1
                && !ConvertUtils.isEmpty(mLocalInfoList.get(index1).getCategoryLocalInfos())
                && mLocalInfoList.get(index1).getCategoryLocalInfos().size() != 1) {
            final SearchCategoryLocalInfo categoryLocalInfo = mLocalInfoList.get(index1);
            for (int i = 0; i < categoryLocalInfo.getCategoryLocalInfos().size(); i++) {
                final SearchChildCategoryLocalInfo childInfo = categoryLocalInfo.getCategoryLocalInfos().get(i);
                if (i == index2) {
                    childInfo.setChecked(1);
                } else {
                    childInfo.setChecked(-1);
                    for (SearchGrandChildCategoryLocalInfo child : childInfo.getCategoryLocalInfos()) {
                        child.setChecked(-1);
                    }
                }
            }
        }
    }

    /**
     * 设置刷新监听
     */
    private void setupRefreshListener() {
        mViewBinding.pullRefreshLayout.setRefreshListener(new RefreshListener() {
            @Override
            public void refresh() {
                if (mPageNum == 1) {
                    Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "已经是第一页，无法刷新");
                } else {
                    performSearch(--mPageNum, getEditText(), false);
                }
                if (mViewBinding != null) {
                    mViewBinding.pullRefreshLayout.finishRefresh();
                }
            }

            @Override
            public void loadMore() {
                if (mPageNum >= maxPageNum || (!ConvertUtils.isEmpty(mSearchResultEntity) && mSearchResultEntity.getPoiList().size() < MAX_LIST_NUMBER)) {
                    Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "没有更多数据了，pageNum: " + mPageNum + " / maxPageNum: " + maxPageNum);
                } else {
                    performSearch(++mPageNum, getEditText(), false);
                }
                if (mViewBinding != null) {
                    mViewBinding.pullRefreshLayout.finishLoadMore();
                }
            }
        });
    }

    /**
     * 关键字搜索
     *
     * @param pageNum 页码
     * @param keyword 关键字
     * @param isReSearch 是否重搜
     */
    public void performSearch(final int pageNum, final String keyword, final boolean isReSearch) {
        if (keyword == null || keyword.trim().isEmpty()) {
            Logger.w(MapDefaultFinalTag.SEARCH_HMI_TAG, "搜索关键字为空，取消搜索");
            return;
        }

        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "执行搜索 - 类型: " + mSearchType
                + ", 关键字: " + keyword + ", 页码: " + pageNum + " ,isReSearch: " + isReSearch);
        if (!ConvertUtils.isNull(mAnimator)) {
            if(mAnimator.isRunning()){
                Logger.e(MapDefaultFinalTag.SEARCH_HMI_TAG, "mAnimator is showing");
            }else{
                showLoading(true);
            }
        }
        if (mSearchType == AutoMapConstant.SearchType.SEARCH_KEYWORD
                || mSearchType == AutoMapConstant.SearchType.AROUND_SEARCH) {
            //沿途搜不支持离线，无需切换离线模式重搜
            ThreadManager.getInstance().removeHandleTask(mTimeoutTask);
            ThreadManager.getInstance().postDelay(mTimeoutTask, 6000);
        }
        switch (mSearchType) {
            case AutoMapConstant.SearchType.SEARCH_KEYWORD:
                if(!ConvertUtils.isEmpty(getClassifyData()) || !ConvertUtils.isEmpty(mQuickValue)){
                    if(!ConvertUtils.isEmpty(mQuickValue)){
                        if(mQuickValue.contains("_")){
                            mScreenViewModel.keywordSearch(mPageNum,mSearchText,mResultEntity.getRetain(),mQuickValue,false);
                        }else{
                            mScreenViewModel.keywordSearchByQuickFilter(mPageNum,mSearchText,mResultEntity.getRetain(),mQuickValue,false);
                        }
                    }else{
                        mScreenViewModel.keywordSearch(pageNum, keyword, mResultEntity.getRetain(),getClassifyData(),false);
                    }
                }else{
                    if (mCityCode != 0) {
                        mScreenViewModel.keywordSearch(pageNum, keyword, mCityCode, false, isReSearch);
                    } else {
                        mScreenViewModel.keywordSearch(pageNum, keyword, isReSearch);
                    }
                }
                break;
            case AutoMapConstant.SearchType.AROUND_SEARCH:
                if(!ConvertUtils.isEmpty(getClassifyData())){
                    mScreenViewModel.aroundSearch(pageNum, keyword, mResultEntity.getRetain(),getClassifyData(),false, mPoiInfoEntity);
                }else{
                    mScreenViewModel.aroundSearch(pageNum, keyword, mPoiInfoEntity, String.valueOf(mRange), isReSearch);
                }
                break;
            case AutoMapConstant.SearchType.ALONG_WAY_SEARCH:
                mScreenViewModel.alongWaySearch(keyword);
                break;
            default:
                Logger.w(MapDefaultFinalTag.SEARCH_HMI_TAG, "未知搜索类型: " + mSearchType);
        }
    }

    private final Runnable mTimeoutTask = new Runnable() {
        @Override
        public void run() {
            if (mScreenViewModel != null) {
                //超时后放弃本次搜索，转成离线搜索
                mScreenViewModel.abortSearch();
                performSearch(mPageNum, mSearchText, true);
            }

        }
    };

    /**
     * 设置搜索框文本并进行搜索
     *
     * @param searchType 搜索类型
     * @param searchText 关键字搜索
     */
    public void setEditText(final int searchType, final String searchText) {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "设置搜索框文本并进行搜索 - 类型: " + searchType + ", 关键字: " + searchText);
        this.mSearchType = searchType;
        this.mSearchText = searchText;
        if ((mSearchType == AutoMapConstant.SearchType.ALONG_WAY_SEARCH
                || mSearchType == AutoMapConstant.SearchType.EN_ROUTE_KEYWORD_SEARCH)
                && (ConvertUtils.equals(searchText, "充电站") || (ConvertUtils.equals(searchText, "加油站")))) {
            //沿途搜并且品类是充电站和加油站时需要批量添加
            mRouteAround = true;
        }
        mViewBinding.searchTextBarView.searchBarTextView.setText(searchText);
        this.mPageNum = 1;
        performSearch(mPageNum, searchText, false);
    }

    public void setPoiInfoEntity(final PoiInfoEntity poiInfo) {
        this.mPoiInfoEntity = poiInfo;
    }

    /**
     * 设置周边搜搜索半径
     * @param range 搜索半径
     */
    public void setRange(final int range) {
        this.mRange = range;
    }

    /**
     * 设置cityCode
     * @param cityCode 城市代码
     */
    public void setCityCode(final int cityCode) {
        this.mCityCode = cityCode;
    }

    /**
     * 设置家的公司 参数
     *
     * @param homeCompanyState 家和公司参数
     */
    public void setHomeCompanyState(final int homeCompanyState) {
        this.mHomeCompanyType = homeCompanyState;
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "setHomeCompanyState - homeCompanyState: " + homeCompanyState);
        if (mAdapter != null) {
            mAdapter.setHomeCompanyType(homeCompanyState);
        }
    }

    /**
     * 退回到搜索结果列表页面时，重新扎标
     * @param name 上次关闭的Fragment界面
     */
    public void reloadPoiMarker(final String name) {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "name: " + name);
        if (!ConvertUtils.isEmpty(mScreenViewModel) && !ConvertUtils.isEmpty(mResultEntity)) {
            if (ConvertUtils.equals(name, "RouteFragment")) {
                mScreenViewModel.addPoiMarker(mResultEntity.getPoiList(), 0);
                updatePoiMarkerVisibleState();
            } else {
                updatePoiMarkerVisibleState();
            }
            if(!ConvertUtils.isNull(mAdapter)){
                mAdapter.clearPoiListChild();
            }
        }
    }

    /**
     * 更新搜索结果
     * @param taskId 请求任务id
     * @param searchResultEntity 搜索结果实体
     */
    public void notifySearchResult(final int taskId, final SearchResultEntity searchResultEntity) {
        if (!ConvertUtils.isNull(mAnimator) && mAnimator.isRunning()) {
            showLoading(false);
        }
        if (mViewBinding != null) {
            mViewBinding.overlayInterceptor.setVisibility(GONE);
        }
        if (ConvertUtils.isEmpty(mScreenViewModel)) {
            return;
        }
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "taskId: " + taskId
                + " currentId: " + mScreenViewModel.getMTaskId());
        if (!ConvertUtils.equals(taskId, mScreenViewModel.getMTaskId()) && mScreenViewModel.getMTaskId() != 0) {
            return;
        }
        ThreadManager.getInstance().removeHandleTask(mTimeoutTask);
        mTaskId = mScreenViewModel.getMTaskId();
        if(mIsFilterViewShow){
            hideFilterPage();
        }
        if(searchResultEntity != null && !searchResultEntity.getIsNetData()){
            mSearchResultEntity = searchResultEntity;
        }
        final String chargeType = ResourceUtils.Companion.getInstance().getString(R.string.st_quick_search_charge);
        final String gasType = ResourceUtils.Companion.getInstance().getString(R.string.st_quick_search_station);
        mChildQuickList = new ArrayList<>();
        // 处理用户搜索意图,意图为充电站显示快筛列表
        if(searchResultEntity != null
                && !ConvertUtils.isEmpty(searchResultEntity.getQueryTypeList())
                && isChargeGasQuery(searchResultEntity.getQueryTypeList(),chargeType)){
            mViewBinding.searchLabelFilter.setVisibility(VISIBLE);
            mChildQuickList = mapCustomLabel(searchResultEntity.getLevel2LocalInfoList(),searchResultEntity);
            for (int i = 0; i < mChildQuickList.size(); i++) {
                mChildQuickList.get(i).setChecked(i == mCurrentSelectedQuick ? 1 : 0);
            }
            if(!ConvertUtils.isNull(mAdapter)){
                ArrayList<String> labelNameList = new ArrayList<>();
                if(mCurrentSelectedQuick > -1 && mCurrentSelectedQuick < mChildQuickList.size()){
                    labelNameList.add(mChildQuickList.get(mCurrentSelectedQuick).getName());
                    mAdapter.setQuickLabel(labelNameList);
                }else {
                    mAdapter.setQuickLabel(getClassifyDataByChecked());
                }
            }else{
                mAdapter.setQuickLabel(new ArrayList<>());
            }
            if(ConvertUtils.isEmpty(mChildQuickList)){
                mViewBinding.searchLabelFilter.setVisibility(GONE);
            }
            mQuickFilterListAdapter.setIconVisible(false);
            mQuickFilterListAdapter.setLabelList(mChildQuickList);
        // 处理用户搜索意图,意图为加油站显示快筛列表
        }else if(searchResultEntity != null
                && ((!ConvertUtils.isEmpty(searchResultEntity.getQueryTypeList())
                && isChargeGasQuery(searchResultEntity.getQueryTypeList(),gasType))
                || isFilterShell(searchResultEntity.getLocalInfoList()))){
            mViewBinding.searchLabelFilter.setVisibility(VISIBLE);
            mChildQuickList = mapCustomGasLabel(searchResultEntity.getLocalInfoList(),searchResultEntity);
            if(mCurrentSelectedQuick != -1){
                refreshLocalInfoListCheckedState(2, mCurrentSelectedQuick + 1);
            }
            mQuickFilterListAdapter.setIconVisible(true);
            mQuickFilterListAdapter.setLabelList(mChildQuickList);
            if(ConvertUtils.isEmpty(mChildQuickList)){
                mViewBinding.searchLabelFilter.setVisibility(GONE);
            }
        // 自营站返回
        }else if(searchResultEntity != null && searchResultEntity.getIsNetData()){
            mViewBinding.searchLabelFilter.setVisibility(VISIBLE);
            mChildQuickList = mapCustomLabel(new ArrayList<>(),searchResultEntity);
            if(mCurrentSelectedQuick > mChildQuickList.size()){
                mCurrentSelectedQuick = mChildQuickList.size() - 1;
            }
            for (int i = 0; i < mChildQuickList.size(); i++) {
                mChildQuickList.get(i).setChecked(i == mCurrentSelectedQuick ? 1 : 0);
            }
            if(!ConvertUtils.isNull(mAdapter) && mCurrentSelectedQuick > -1 && !ConvertUtils.isEmpty(mChildQuickList.size())){
                ArrayList<String> labelNameList = new ArrayList<>();
                labelNameList.add(mChildQuickList.get(mCurrentSelectedQuick).getName());
                mAdapter.setQuickLabel(labelNameList);
            }else{
                mAdapter.setQuickLabel(new ArrayList<>());
            }
            if(ConvertUtils.isEmpty(mChildQuickList)){
                mViewBinding.searchLabelFilter.setVisibility(GONE);
            }
            mQuickFilterListAdapter.setLabelList(mChildQuickList);
        }else{
            mQuickValue = "";
            mViewBinding.searchLabelFilter.setVisibility(GONE);
        }
        if (searchResultEntity == null || searchResultEntity.getPoiList().isEmpty()) {
            if (searchResultEntity != null
                    && searchResultEntity.getPoiType() == 0
                    && !ConvertUtils.isEmpty(MapDataPackage.getInstance().getAllDownLoadedList())) {
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"mSearchType: ",mSearchType,"code: ",searchResultEntity.getCode());
                // 顺路搜不支持离线搜索，当顺路搜无网络时提醒网络异常
                if(mSearchType == AutoMapConstant.SearchType.ALONG_WAY_SEARCH && searchResultEntity.getCode() == 33554433){
                    ToastUtils.Companion.getInstance().showCustomToastView("网络异常，请检查网络后重试");
                }else{
                    //无网无结果存在离线城市但无推荐结果
                    ToastUtils.Companion.getInstance().showCustomToastView("抱歉，未找到结果");
                }
            } else {
                //当任何需要网络相应才能完成的操作，但网络异常时，统一给予以下提示
                if(!ConvertUtils.isNull(searchResultEntity) && searchResultEntity.getPoiList().isEmpty() && searchResultEntity.getCode() != 33554433){
                    ToastUtils.Companion.getInstance().showCustomToastView("抱歉，未找到结果");
                }else{
                    ToastUtils.Companion.getInstance().showCustomToastView("网络异常，请检查网络后重试");
                }
            }
            showLoading(false);
            if (null != mAdapter) {
                mAdapter.clearList();
            }
            if (mViewBinding != null) {
                ThreadManager.getInstance().postDelay(mOfflineRunnable, 3500);
                //搜索无数据时，展示无结果页面
                mViewBinding.searchResultNoData.setVisibility(VISIBLE);
                // 提示网络错误：关键字搜（离线-无离线数据）顺路搜因为不支持离线搜（返回错误码33554433）
                if ((searchResultEntity != null
                        && searchResultEntity.getPoiType() == 0
                        && ConvertUtils.isEmpty(MapDataPackage.getInstance().getAllDownLoadedList()))
                        || (searchResultEntity != null
                        && mSearchType == AutoMapConstant.SearchType.ALONG_WAY_SEARCH
                        && searchResultEntity.getCode() == 33554433)) {
                    mViewBinding.searchResultNoData.setText(R.string.search_offline_no_city_hint);
                } else {
                    mViewBinding.searchResultNoData.setText(R.string.sug_search_result_no_data);
                }
            }

            return;
        }
        //有数据时，隐藏异常提示界面
        mViewBinding.searchResultNoData.setVisibility(GONE);

        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "mSearchType: " + mSearchType);

        if (mScreenViewModel.isAlongWaySearch()) {
            mViewBinding.routeRightTabListChargeScene.setVisibility(VISIBLE);
//            updateSearchContainerMarginBottom(true);
            if(!ConvertUtils.isEmpty(searchResultEntity.getKeyword())){
                final String queryType = com.android.utils.ResourceUtils.Companion.getInstance().getString(R.string.st_quick_search_charge);
                mViewBinding.routeRightTabListChargeScene.setSearchCharge(queryType.equals(searchResultEntity.getKeyword()));
            }
            updateRouteList();
            if (mAdapter != null) {
                mAdapter.updateAlongList(mGasChargeAlongList);
            }
            if ((mSearchType == AutoMapConstant.SearchType.ALONG_WAY_SEARCH
                    || mSearchType == AutoMapConstant.SearchType.EN_ROUTE_KEYWORD_SEARCH) && mRouteAround) {

                List<PoiInfoEntity> poiInfoEntities = searchResultEntity.getPoiList();
                mRoutePackage.setRouteAlongSearch(true);
                if (mRoutePackage.isRouteTips()) {
                    clearRouteChargePoiUi();
                    updateChargeProgress();

                    //初始化进度条扎点
                    for (PoiInfoEntity poiInfoEntity : poiInfoEntities) {
                        if (isBelongAlongList(poiInfoEntity)) {
                            addRouteChargePoiUi(poiInfoEntity, (float) (poiInfoEntity.getSort_distance()) / mRouteTotalDistance);
                        }
                    }
                }

            }
            mViewBinding.routeRightTabListChargeScene.registerRouteSelectObserver(TAG, this);

        } else {
//            updateSearchContainerMarginBottom(false);
            mViewBinding.routeRightTabListChargeScene.setVisibility(GONE);
        }

        if (mSearchType == AutoMapConstant.SearchType.ALONG_WAY_SEARCH) {
            mViewBinding.routeRightTabListChargeScene.highlightAlongTab();
        }

        if (searchResultEntity.getPoiType() == 0) {
            final CityDataInfo cityDataInfo;
            if(!ConvertUtils.isEmpty(searchResultEntity.getPoiList())) {
                final PoiInfoEntity poiInfoEntity = searchResultEntity.getPoiList().get(0);
                cityDataInfo = mScreenViewModel.getCityInfo(poiInfoEntity.getAdCode());
            } else {
                cityDataInfo = mScreenViewModel.getCityInfo(mScreenViewModel.getAcCode());
            }
            if (cityDataInfo != null) {
                int currentAdCode = mScreenViewModel.getAcCode();
                int searchAdCode = cityDataInfo.getUpperAdcode();
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "城市数据信息: " + cityDataInfo.getName() + "，搜索的城市编码: " + searchAdCode + " ,当前城市编码："+currentAdCode);
                mViewBinding.searchOfflineHint.setVisibility(VISIBLE);
                final CityDataInfo searchCityInfo = mScreenViewModel.getCityInfo(searchAdCode);
                if(currentAdCode == searchAdCode){
                    mViewBinding.searchOfflineHint.setText(getContext().getString(R.string.search_offline_hint, searchCityInfo.getName()));
                }else{
                    final CityDataInfo queryCityInfo = mScreenViewModel.getCityInfo(currentAdCode);
                    mViewBinding.searchOfflineHint.setText(getContext().getString(R.string.search_offline_hint_extend, queryCityInfo.getName(), searchCityInfo.getName()));
                }
            }
        } else {
            mViewBinding.searchOfflineHint.setVisibility(GONE);
        }
        mResultEntity = searchResultEntity;
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "notifySearchResult name: " + searchResultEntity.getKeyword());
        mSearchType = searchResultEntity.getSearchType();
        if (!ConvertUtils.isEmpty(searchResultEntity.getPoiList()) && searchResultEntity.getPoiList().size() == 1 && mPageNum == 1) {
            //只有一个搜索结果时，直接跳转结果界面
            final Fragment fragment = (Fragment) ARouter.getInstance().build(RoutePath.Search.POI_DETAILS_FRAGMENT).navigation();
            final int poiType = getPoiType(mHomeCompanyType);
            closeCurrentFragment(false);
            Bundle bundle = SearchFragmentFactory.createPoiDetailsFragment(
                    AutoMapConstant.SourceFragment.SEARCH_RESULT_FRAGMENT, poiType, searchResultEntity.getPoiList().get(0));
            bundle.putBoolean("IS_END", mIsEnd);
            addFragment((BaseFragment) fragment, bundle,false);
            return;
        }
        setMaxPageNum(searchResultEntity.getMaxPageNum());
        if (mAdapter != null) {
            mAdapter.notifyList(searchResultEntity);
            updatePoiMarkerVisibleState();
            mViewBinding.recyclerSearchResult.scrollToPosition(0);
        }
        if (mIsFilterViewShow) {
            mViewBinding.searchTextBarView.searchBarTextView.setText(getContext().getString(
                    R.string.filter_result, mSearchText, searchResultEntity.getTotal()));
        }
        if (TextUtils.isEmpty(mViewBinding.searchTextBarView.searchBarTextView.getText())) {
            mSearchText = searchResultEntity.getKeyword();
            mViewBinding.searchTextBarView.searchBarTextView.setText(searchResultEntity.getKeyword());
        }
        mLocalInfoList = searchResultEntity.getLocalInfoList();
        if (!ConvertUtils.isEmpty(mLocalInfoList)) {
            mViewBinding.searchTextBarView.csFilter.setVisibility(VISIBLE);
            mViewBinding.searchTextBarView.ivSearch.setVisibility(GONE);
        } else {
            mViewBinding.searchTextBarView.csFilter.setVisibility(GONE);
        }
    }

    public void notifySearchResultNetError(int taskId,String message){
        if(ConvertUtils.isNull(mScreenViewModel)){
            return;
        }
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"error message: " + message + "--taskId: "+taskId + "--currentTaskId: "+mScreenViewModel.getMTaskId());
        if ((!ConvertUtils.equals(taskId, mScreenViewModel.getMTaskId()) && mScreenViewModel.getMTaskId() != 0) || mViewBinding == null) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"error");
            return;
        }
        showLoading(false);
        mViewBinding.searchResultNoData.setVisibility(VISIBLE);
    }

    /**
     * 静默搜索,仅用于显示泛搜结果数量,不进行页面更新
     *
     * @param searchResultEntity 数据实体类
     * @param taskId 任务id
     */
    public void notifySilentSearchResult(final int taskId, final SearchResultEntity searchResultEntity) {
        // 1055723 容错处理
        if(ConvertUtils.isNull(mScreenViewModel)){
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"mScreenViewModel is Null");
            return;
        }
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "taskId: " + taskId
                + " currentId: " + mScreenViewModel.getMTaskId());
        if ((!ConvertUtils.equals(taskId, mScreenViewModel.getMTaskId()) && mScreenViewModel.getMTaskId() != 0) || mViewBinding == null) {
            return;
        }
        if (searchResultEntity == null || searchResultEntity.getPoiList() == null || searchResultEntity.getPoiList().isEmpty()) {
            ToastUtils.Companion.getInstance().showCustomToastView("暂无数据");
            if (mViewBinding != null) {
                mViewBinding.searchTextBarView.searchBarTextView.setText(getContext().getString(R.string.filter_result, mSearchText, 0));
            }
            return;
        }
        if (mIsFilterViewShow) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "notifySilentSearchResult total: " + searchResultEntity.getTotal());
            if (mViewBinding != null) {
                mViewBinding.searchTextBarView.searchBarTextView.setText(getContext().getString(
                        R.string.filter_result, mSearchText, searchResultEntity.getTotal()));
            }
        }
    }

    /**
     * 语音筛选搜索回调，收到此回调后根据筛选条件进行一次筛选搜索
     * @param sortValue 筛选条件
     */
    public void onVoicePoiSort(final String sortValue) {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onVoicePoiSort1: " + sortValue);
        if (mScreenViewModel != null && mResultEntity != null) {
            mScreenViewModel.keywordSearch(mPageNum, mSearchText, mResultEntity.getRetain(), getVoiceClassifyData(sortValue), false);
        }
    }

    /**
     * 语音筛选搜索回调，收到此回调后根据筛选条件进行一次筛选搜索
     * @param sortValue 筛选条件
     * @param point 目标经纬度
     */
    public void onVoicePoiSort(final String sortValue, final GeoPoint point) {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onVoicePoiSort2: " + sortValue);
        if (mScreenViewModel != null && mResultEntity != null) {
            PoiInfoEntity poiInfo = new PoiInfoEntity()
                    .setPoint(point);
            mScreenViewModel.aroundSearch(mPageNum, mSearchText, mResultEntity.getRetain(), getVoiceClassifyData(sortValue), false, poiInfo);

        }
    }

    /**
     * 图层点击事件回调,点击后更新扎标状态
     * @param poiInfoEntity 点击下标
     */
    public void onMarkClickCallBack(final PoiInfoEntity poiInfoEntity) {
        if (!ConvertUtils.isEmpty(mScreenViewModel) && !ConvertUtils.isEmpty(mResultEntity)) {
            final List<PoiInfoEntity> poiInfoEntities = mResultEntity.getPoiList();
            int index = 0;
            if (!ConvertUtils.isEmpty(poiInfoEntities)) {
                // 遍历所有可见的item
                for (int i = 0; i < poiInfoEntities.size(); i++) {
                    if (!ConvertUtils.isEmpty(poiInfoEntities)) {
                        if (ConvertUtils.equals(poiInfoEntities.get(i).getPid(), poiInfoEntity.getPid())) {
                            index = i;
                        }
                        poiInfoEntities.get(i).setMIsVisible(false);
                    }
                }
                mScreenViewModel.setSelectIndex(poiInfoEntity, index, mSearchType);
                mScreenViewModel.updatePoiMarker(poiInfoEntities, 0, false);
            }
        }


    }

    public void clearLabelVisibleState() {
        if (!ConvertUtils.isEmpty(mScreenViewModel) && !ConvertUtils.isEmpty(mResultEntity)) {
            final List<PoiInfoEntity> poiInfoEntities = mResultEntity.getPoiList();
            if (!ConvertUtils.isEmpty(poiInfoEntities)) {
                // 遍历所有可见的item
                for (int i = 0; i < poiInfoEntities.size(); i++) {
                    if (!ConvertUtils.isEmpty(poiInfoEntities)) {
//                        if (ConvertUtils.equals(poiInfoEntities.get(i).getPid(), poiInfoEntities.get(i).getPid())) {
                        poiInfoEntities.get(i).setMIsVisible(false);
//                        }
                    }
                }
                mScreenViewModel.updatePoiMarker(poiInfoEntities, 0, false);
            }
        }
    }

    /**
     * 设置最大页数
     *
     * @param maxPageNum 最大页数
     */
    private void setMaxPageNum(final int maxPageNum) {
        this.maxPageNum = maxPageNum;
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "更新 maxPageNum: " + maxPageNum);
    }

    /**
     * 获取输入框内容
     *
     * @return 输入框内容
     */
    private String getEditText() {
        final CharSequence text = mViewBinding.searchTextBarView.searchBarTextView.getText();
        return text != null ? text.toString().trim() : "";
    }

    /**
     * 清空输入框内容
     */
    public void clearEditText() {
        mViewBinding.searchTextBarView.searchBarTextView.setText("");
    }


    /**
     * 是否需要添加途经点
     *
     * @return true: 需要添加途经点，false: 不需要添加途经点
     */
    public boolean isAlongWaySearch() {
        return mScreenViewModel.isAlongWaySearch();
    }

    /**
     * 获取分类数据
     *
     * @return 分类数据
     */
    private String getClassifyData() {
        if (ConvertUtils.isEmpty(mLocalInfoList)) {
            return "";
        }
        final StringBuilder stringBuilder = new StringBuilder();
        for (SearchCategoryLocalInfo searchCategoryLocalInfo : mLocalInfoList) {
            for (SearchChildCategoryLocalInfo searchChildCategoryLocalInfo : searchCategoryLocalInfo.getCategoryLocalInfos()) {
                if (searchChildCategoryLocalInfo.getChecked() == 1
                        && !ConvertUtils.isEmpty(searchChildCategoryLocalInfo.getValue())) {
                    stringBuilder.append(searchChildCategoryLocalInfo.getValue());
                    stringBuilder.append(APPEND);
                }
                for (SearchGrandChildCategoryLocalInfo searchChildCategoryLocalInfo1 : searchChildCategoryLocalInfo.getCategoryLocalInfos()) {
                    if (searchChildCategoryLocalInfo1.getChecked() == 1
                            && !ConvertUtils.isEmpty(searchChildCategoryLocalInfo1.getValue())) {
                        stringBuilder.append(searchChildCategoryLocalInfo1.getValue());
                        stringBuilder.append(APPEND);
                    }
                }
            }
        }
        if (stringBuilder.toString().endsWith(APPEND)) {
            stringBuilder.deleteCharAt(stringBuilder.length() - 1);
        }
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "mSearchCategoryLocalInfos getClassifyData: " + stringBuilder.toString());
        return stringBuilder.toString();
    }

    /**
     * 获取分类数据
     *
     * @return 分类数据
     */
    private ArrayList<String> getClassifyDataByChecked() {
        if (ConvertUtils.isEmpty(mLocalInfoList)) {
            return new ArrayList<>();
        }
        final ArrayList<String> labelNameList = new ArrayList<>();
        for (SearchCategoryLocalInfo searchCategoryLocalInfo : mLocalInfoList) {
            for (SearchChildCategoryLocalInfo searchChildCategoryLocalInfo : searchCategoryLocalInfo.getCategoryLocalInfos()) {
                if (searchChildCategoryLocalInfo.getChecked() == 1
                        && !ConvertUtils.isEmpty(searchChildCategoryLocalInfo.getValue()) && !"searchlist_charging_mode_none".equals(searchChildCategoryLocalInfo.getValue())) {
                    labelNameList.add(searchChildCategoryLocalInfo.getName());
                }
                for (SearchGrandChildCategoryLocalInfo searchChildCategoryLocalInfo1 : searchChildCategoryLocalInfo.getCategoryLocalInfos()) {
                    if (searchChildCategoryLocalInfo1.getChecked() == 1
                            && !ConvertUtils.isEmpty(searchChildCategoryLocalInfo1.getValue()) && !"searchlist_charging_distance_none".equals(searchChildCategoryLocalInfo1.getValue())) {
                        labelNameList.add(searchChildCategoryLocalInfo1.getName());
                    }
                }
            }
        }
        return labelNameList;
    }

    /**
     * 获取语音筛选数据
     * @param sortValue 分类名称
     * @return 筛选数据请求value
     */
    private String getVoiceClassifyData(final String sortValue) {
        String classifyData = "";
        for (SearchCategoryLocalInfo searchCategoryLocalInfo : mLocalInfoList) {
            for (SearchChildCategoryLocalInfo searchChildCategoryLocalInfo : searchCategoryLocalInfo.getCategoryLocalInfos()) {
                if (searchChildCategoryLocalInfo.getName().equals(sortValue)) {
                    classifyData = searchChildCategoryLocalInfo.getValue();
                    return classifyData;
                }
            }
        }
        return classifyData;
    }

    /**
     * clear data
     */
    public void clear() {
        mPoiInfoEntity = null;
        mResultEntity = null;
    }

    public int getTaskId() {
        return mTaskId;
    }

    public SearchResultEntity getResultEntity() {
        return mResultEntity;
    }

    public void setMIsEnd(final boolean isEnd) {
        this.mIsEnd = isEnd;
        if (mAdapter != null) {
            mAdapter.setIsEnd(isEnd);
        }

    }

    @Override
    public void onTabListGasChargeClick(final int tabIndex) {
        mScreenViewModel.onTabListGasChargeClick(mResultEntity.getKeyword(), tabIndex);
        showLoading(true);
        mViewBinding.routeRightTabListChargeScene.updateUi();
        mViewBinding.overlayInterceptor.setVisibility(VISIBLE);
        if (mViewBinding.routeChargeListAlongWaySure == null || mViewBinding.routeChargeListAlongWayCancel == null
                || mViewBinding.layoutRouteChargeProgress == null) {
            Logger.e(TAG, "view is null");
            return;
        }
        if (tabIndex != 0) {
            mSearchType = AutoMapConstant.SearchType.AROUND_SEARCH;
            mViewBinding.routeChargeListAlongWaySure.setVisibility(View.GONE);
            mViewBinding.routeChargeListAlongWayCancel.setVisibility(View.GONE);
            mViewBinding.layoutRouteChargeProgress.setVisibility(View.GONE);
            mRoutePackage.setRouteAlongSearch(false);
        } else {
            mSearchType = AutoMapConstant.SearchType.EN_ROUTE_KEYWORD_SEARCH;
        }

    }

    /**
     * @param b 从导航进的搜索结果页面
     */
    public void setNaviControl(final boolean b) {
        mIsOpenFromNavi = b;
    }

    private void sendBuryPointForListSelect(int index, String value){
        BuryProperty buryProperty = new BuryProperty.Builder()
                .setParams(BuryConstant.ProperType.BURY_KEY_SEARCH_CONTENTS, value)
                .setParams(BuryConstant.ProperType.BURY_KEY_HOME_PREDICTION, Integer.toString(index))
                .build();
        BuryPointController.getInstance().setBuryProps(buryProperty);
    }

    // 判断用户搜索意图是否是充电站或者加油站
    private boolean isChargeGasQuery(ArrayList<String> list,String queryType){
        final double threshold = 0.5;
        int count = 0;
        for (int i = 0; i < list.size(); i++) {
            String type = list.get(i);
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"type: "+type);
            if(queryType.equals(type.split("=")[0].trim())
                    && !type.split("=")[1].trim().contains(";")
                    && Double.parseDouble(type.split("=")[1].trim()) > threshold){
                count++;
            }
        }
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"count: "+count);
        return count > 0;
    }

    @Override
    public void onDestroy() {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"onDestroy: " + mScreenViewModel);
        if (mScreenViewModel != null) {
            //切换桌面地图 应用重走声明周期  onDestroy中应该调用删除扎标方法
            mScreenViewModel.clearListLabel();
        }
        super.onDestroy();
        if (!ConvertUtils.isNull(mAnimator)) {
            mAnimator.cancel();
        }
        ThreadManager.getInstance().removeHandleTask(mTimeoutTask);
        ThreadManager.getInstance().removeHandleTask(mOfflineRunnable);
    }

    /**
     * 处理快筛标签列表
     * @param list 二筛列表
     * @return
     */
    private List<SearchChildCategoryLocalInfo> mapCustomLabel(List<SearchCategoryLocalInfo> list,SearchResultEntity searchResultEntity) {
        // 高配车型
        List<SearchChildCategoryLocalInfo> childListByHigh = new ArrayList<>();
        // 低配车型
        List<SearchChildCategoryLocalInfo> childListByLow = new ArrayList<>();
        int brand = mScreenViewModel.getBrand();
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"brand: "+brand);
        boolean isPowerType = mScreenViewModel.powerType() == 1 || mScreenViewModel.powerType() == 2;
        boolean isOffline = searchResultEntity.getPoiType() == 0;
        List<SearchChildCategoryLocalInfo> childList = ConvertUtils.isEmpty(list) ? new ArrayList<>() : list.get(0).getCategoryLocalInfos();
        if (isPowerType) {
            // 添加免费停车快筛
            SearchChildCategoryLocalInfo parkItem = new SearchChildCategoryLocalInfo()
                    .setName(ResourceUtils.Companion.getInstance().getString(R.string.filter_select_free_park))
                    .setValue("searchlist_charging_type_free_parking");
            childListByLow.add(parkItem);
            // 添加快充快筛
            SearchChildCategoryLocalInfo chargeItem = new SearchChildCategoryLocalInfo()
                    .setName(ResourceUtils.Companion.getInstance().getString(R.string.filter_select_fast))
                    .setValue("searchlist_charging_mode_fast");
            childListByHigh.add(chargeItem);
        }
        // 凯迪需要过滤掉二筛中的快筛标签
        if(brand == 2 && !ConvertUtils.isEmpty(childList)){
            childList.removeIf(value -> ResourceUtils.Companion.getInstance().getString(R.string.filter_select_fast).equals(value.getMValue()));
        }
        // 别克：性价比车型1，凯迪：高品质车型2
        childListByHigh.addAll(ConvertUtils.isEmpty(list) ? new ArrayList<>() : childList);
        childListByLow.addAll(ConvertUtils.isEmpty(list) ? new ArrayList<>() : childList);
        // 添加自营快筛
        if (isPowerType && !isOffline) {
            SearchChildCategoryLocalInfo chargeItem = new SearchChildCategoryLocalInfo()
                    .setName(ResourceUtils.Companion.getInstance().getString(R.string.search_charge_self))
                    .setValue("charge");
            childListByHigh.add(chargeItem);
            childListByLow.add(chargeItem);
        }
        return brand == 2 ? childListByHigh : childListByLow;
    }

    private List<SearchChildCategoryLocalInfo> mapCustomGasLabel(List<SearchCategoryLocalInfo> list,SearchResultEntity searchResultEntity) {
        List<SearchChildCategoryLocalInfo> childList = new ArrayList<>();
        if(!ConvertUtils.isEmpty(list)){
            for (int i = 0; i < list.size(); i++) {
                if(i == 2){
                    childList = list.get(i).getCategoryLocalInfos().stream()
                                    .filter(value -> !"searchlist_traffic_gas_brand_all".equals(value.getValue()))
                                    .collect(Collectors.toCollection(ArrayList::new));
                }
            }
        }
        return childList;
    }

    private boolean isFilterShell(List<SearchCategoryLocalInfo> list){
        if(mSearchType == AutoMapConstant.SearchType.EN_ROUTE_KEYWORD_SEARCH || mSearchType == AutoMapConstant.SearchType.ALONG_WAY_SEARCH){
            return false;
        }
        int count = 0;
        // 加油站 ”壳牌“快捷筛选，意图不是加油站，通过筛选列表判断意图
        if(!ConvertUtils.isEmpty(list)){
            for (int i = 0; i < list.size(); i++) {
                for (int j = 0; j < list.get(i).getCategoryLocalInfos().size(); j++) {
                    if("searchlist_traffic_gas_brand_shell".equals(list.get(i).getCategoryLocalInfos().get(j).getValue())){
                        count++;
                    }
                }
            }
        }
        return count > 0;
    }

    private void hideFilterPage(){
        mViewBinding.searchFilterView.searchFilterRoot.setVisibility(GONE);
        updateChargeList();
        mIsFilterViewShow = false;
        updateChargeList();
        mViewBinding.searchTextBarView.searchBarTextView.setText(mSearchText);
        if(ConvertUtils.isEmpty(mResultEntity.getPoiList())){
            mViewBinding.searchResultNoData.setVisibility(VISIBLE);
        }else{
            mViewBinding.pullRefreshLayout.setVisibility(VISIBLE);
        }
        if(!ConvertUtils.isEmpty(mChildQuickList)){
            mViewBinding.searchLabelFilter.setVisibility(VISIBLE);
        }
    }

//    private void updateSearchContainerMarginBottom(boolean isAlongWay) {
//        if (mSearchContainer != null) {
//            LinearLayout.LayoutParams lp = (LinearLayout.LayoutParams) mSearchContainer.getLayoutParams();
//            if (isAlongWay) {
//                lp.height = getResources().getDimensionPixelSize(com.sgm.navi.ui.R.dimen.dp_733);
//            } else {
//                lp.height = LinearLayout.LayoutParams.WRAP_CONTENT;
//            }
//            mSearchContainer.setLayoutParams(lp);
//        }
//    }


    //------------算路沿途搜***************************************************/
    private boolean mRouteAround = false;
    private List<RouteParam> mGasChargeAlongList = new ArrayList<>();
    private List<RouteParam> mRouteGasChargeAlongList = new ArrayList<>();
    private Map<PoiInfoEntity, View> mRouteChargeProgressViews;
    private List<Integer> mChargePoiDistanceList = new ArrayList<>();
    private long mRouteTotalDistance;
    private long mExhaustDistance;
    private long mRouteExhaustDistance;
    private final int mViewWidth = getResources().getDimensionPixelSize(R.dimen.route_supplement_width);
    private final int mTotalWidth = getResources().getDimensionPixelSize(R.dimen.route_charge_progress_total);
    private int mEndWidth;

    public void setRouteAround(boolean routeAround) {
        mRouteAround = routeAround;
    }

    public void updateChargeProgress() {
        if (mViewBinding.layoutRouteChargeProgress != null && mViewBinding.routeChargeTotalMileage != null
                && mViewBinding.routeChargeExhaustionPoint != null && mViewBinding.routeChargeProgress != null) {
            if (mRoutePackage.isRouteTips() && mRouteAround) {
                final RouteLineInfo routeLineInfo = mRoutePackage.getSelectLineInfo(MapType.MAIN_SCREEN_MAIN_MAP);
                if (getCurrentIndex() == null || routeLineInfo == null) {
                    Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "routeLineInfo is null");
                    return;
                }
                mViewBinding.routeChargeTotalMileage.setText(TimeUtils.getInstance().getDistanceMsg(routeLineInfo.getMDistance()));
                mViewBinding.routeChargeTotalMileage.post(() -> {
                    if (mViewBinding != null) {
                        mEndWidth = mViewBinding.routeChargeTotalMileage.getWidth() + mViewWidth/2;
                    }
                });
                final EvRangeOnRouteInfo evRangeOnRouteInfo = getRangeOnRouteInfo(getCurrentIndex());
                mRouteTotalDistance = routeLineInfo.getMDistance();
                if (evRangeOnRouteInfo == null || evRangeOnRouteInfo.isMCanArrived()) {
                    mViewBinding.routeChargeExhaustionPoint.setVisibility(View.GONE);
                    mViewBinding.routeChargeProgress.setProgress(100);
                    mExhaustDistance = mRouteTotalDistance;
                    mRouteExhaustDistance = mRouteTotalDistance;
                } else {
                    mViewBinding.routeChargeExhaustionPoint.setVisibility(View.VISIBLE);
                    mExhaustDistance = mRouteTotalDistance - evRangeOnRouteInfo.getMRemainRangeDistance();
                    mRouteExhaustDistance = mExhaustDistance;
                    final int progress = Math.round(((float) (mExhaustDistance) / mRouteTotalDistance) * 100);
                    mViewBinding.routeChargeProgress.setProgress(progress);
                    updateRouteChargeExhaustUi(progress / 100.0f);
                }
                mViewBinding.layoutRouteChargeProgress.setVisibility(View.VISIBLE);
            } else {
                mViewBinding.layoutRouteChargeProgress.setVisibility(View.GONE);
            }
        }
    }

    /***
     * 更新路径
     * @param progress 百分比
     */
    public void updateRouteChargeExhaustUi(final float progress) {
        if (null == mViewBinding.routeChargeExhaustionPoint) {
            Logger.e(TAG, "mRouteChargeGasListPageView = null");
            return;
        }
        ThreadManager.getInstance().postUi(() -> {
            if(!ConvertUtils.isNull(mViewBinding)){
                final ConstraintLayout.LayoutParams layoutParams = (ConstraintLayout.LayoutParams) mViewBinding.routeChargeExhaustionPoint.getLayoutParams();
                layoutParams.horizontalBias = progress;
                mViewBinding.routeChargeExhaustionPoint.setLayoutParams(layoutParams);
            }
        });
    }

    /**
     * 同时添加多个途径带点
     * */
    public void startAllRequest() {
        if (mGasChargeAlongList.size() > 5) {
            ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.add_via_failure));
            return;
        }
        mRoutePackage.requestManyVia(MapType.MAIN_SCREEN_MAIN_MAP, mGasChargeAlongList);
    }

    public void routeClickEvent(final PoiInfoEntity poiInfoEntity) {
        if (mAdapter == null || poiInfoEntity == null) {
            return;
        }
        int position = mAdapter.getTargetIndex(poiInfoEntity);
        int pointType = mScreenViewModel.getPointTypeCode(poiInfoEntity.getPointTypeCode());
        if (isBelongAlongList(poiInfoEntity) || mRoutePackage.isStartOrEndRouteParam(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity)) {
            gasChargeRemoveMode(poiInfoEntity);
            for (PoiInfoEntity poiInfoEntity1 : mResultEntity.getPoiList()) {
                if (ConvertUtils.equals(poiInfoEntity1.getPid(), poiInfoEntity.getPid())) {
                    poiInfoEntity1.setMIsVisible(false);
                }
            }
        } else {
            gasChargeAddMode(poiInfoEntity);
            for (PoiInfoEntity poiInfoEntity1 : mResultEntity.getPoiList()) {
                if (ConvertUtils.equals(poiInfoEntity1.getPid(), poiInfoEntity.getPid())) {
                    poiInfoEntity1.setMIsVisible(true);
                }
            }
        }
        if (pointType == AutoMapConstant.PointTypeCode.CHARGING_STATION) {
            mScreenViewModel.setEnrouteSelect(LayerPointItemType.SEARCH_POI_ALONG_ROUTE_ADD, position, mResultEntity.getPoiList());
        }
        if (mAdapter != null) {
            mAdapter.poiDetailsUpdate(mGasChargeAlongList);
        }
    }

    public void routeClickEvent(final PoiInfoEntity poiInfoEntity, final int position) {
        if (poiInfoEntity == null) {
            return;
        }
        int pointType = mScreenViewModel.getPointTypeCode(poiInfoEntity.getPointTypeCode());
        if (isBelongAlongList(poiInfoEntity) || mRoutePackage.isStartOrEndRouteParam(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity)) {
            gasChargeRemoveMode(poiInfoEntity);
            poiInfoEntity.setMIsVisible(false);
        } else {
            gasChargeAddMode(poiInfoEntity);
        }
        if (pointType == AutoMapConstant.PointTypeCode.CHARGING_STATION) {
            mScreenViewModel.setEnrouteSelect(LayerPointItemType.SEARCH_POI_ALONG_ROUTE_ADD, position, mResultEntity.getPoiList());
        }
        if (mAdapter != null) {
            mAdapter.updateAlongList(mGasChargeAlongList, position);
        }
    }

    public void updateRouteList() {
        mGasChargeAlongList.clear();
        mRouteGasChargeAlongList.clear();
        final List<RouteParam> allPoiParamList = mRoutePackage.getAllPoiParamList(MapType.MAIN_SCREEN_MAIN_MAP);
        if (!ConvertUtils.isEmpty(allPoiParamList) && allPoiParamList.size() >= 2) {
            allPoiParamList.remove(0);
            allPoiParamList.remove(allPoiParamList.size() - 1);
        } else {
            Logger.e(TAG, "ERROR VIA LIST");
            return;
        }
        mGasChargeAlongList.addAll(allPoiParamList);
        mRoutePackage.setGasChargeAlongList(mGasChargeAlongList);
        mRouteGasChargeAlongList.addAll(allPoiParamList);
        updateChargeList();

    }

    /**
     * 充电站本地添加
     * @param poiInfoEntity  请求参数
     * */
    public void gasChargeAddMode(final PoiInfoEntity poiInfoEntity) {
        if (mGasChargeAlongList.size() >= 5) {
            ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.add_via_failure));
            return;
        }
        if (mRoutePackage.isRouteTips()) {
            //绘制充电站在进度条上的扎点
            addRouteChargePoiUi(poiInfoEntity, (float) (poiInfoEntity.getSort_distance()) / mRouteTotalDistance);
        }
        mGasChargeAlongList.add(mRoutePackage.getRouteParamFromPoiInfoEntity(poiInfoEntity, RoutePoiType.ROUTE_POI_TYPE_WAY));
        mRoutePackage.setGasChargeAlongList(mGasChargeAlongList);
        poiInfoEntity.setMIsVisible(true);
        updateChargeList();
    }

    /***
     * 添加充电站内容
     */
    public void updateExhaustDistance() {
        if (mExhaustDistance == mRouteTotalDistance) {
            return;
        }
        mExhaustDistance = mRouteExhaustDistance;
        boolean foundSmaller;
        int farthestDistance = 0;
        do {
            //没有添加充电站
            if (mChargePoiDistanceList == null && mChargePoiDistanceList.isEmpty()) {
                break;
            }
            foundSmaller = false;
            int maxValueSmallerThanX = farthestDistance;

            for (int value : mChargePoiDistanceList) {
                if (value < mExhaustDistance && value > maxValueSmallerThanX) {
                    maxValueSmallerThanX = value;
                }
            }

            if (maxValueSmallerThanX != farthestDistance) {
                mExhaustDistance = (long) (maxValueSmallerThanX + BevPowerCarUtils.getInstance().arrivingPercent
                        * BevPowerCarUtils.getInstance().batterToDistance);
                foundSmaller = true;
            }
            //添加充电站都远于能量耗尽点
            if (maxValueSmallerThanX == 0) {
                mExhaustDistance = mRouteExhaustDistance;
            }
            farthestDistance = maxValueSmallerThanX;

        } while (foundSmaller);

        //算路能耗距离更远
        mExhaustDistance = Math.max(mExhaustDistance, mRouteExhaustDistance);
        mViewBinding.routeChargeExhaustionPoint.setVisibility(View.VISIBLE);
        int progress = Math.round(((float) (mExhaustDistance) / mRouteTotalDistance) * 100);
        if (progress >= 100) {
            mViewBinding.routeChargeExhaustionPoint.setVisibility(View.GONE);
            progress = 100;
        }
        mViewBinding.routeChargeProgress.setProgress(progress);
        updateRouteChargeExhaustUi(progress / 100.0f);
    }


    /**
     * 充电站本地移除
     * @param poiInfoEntity  请求参数
     * */
    public void gasChargeRemoveMode(final PoiInfoEntity poiInfoEntity) {
        if (mRoutePackage.isStartOrEndRouteParam(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity)) {
            ToastUtils.Companion.getInstance().showCustomToastView(
                    ResourceUtils.Companion.getInstance().getString(R.string.route_error_add_start_end));
            return;
        }
        if (!mRoutePackage.isRouteTips()) {
            RouteParam routeParams = new RouteParam();
            for (RouteParam routeParam : mGasChargeAlongList) {
                if (mRoutePackage.isTheSamePoi(routeParam, poiInfoEntity)) {
                    routeParams = routeParam;
                    break;
                }
            }
            mGasChargeAlongList.remove(routeParams);
            mRoutePackage.setGasChargeAlongList(mGasChargeAlongList);

            updateChargeList();
            return;
        }
        PoiInfoEntity addPoiInfo = null;
        if (mRouteChargeProgressViews != null && !mRouteChargeProgressViews.isEmpty()) {
            for (PoiInfoEntity poiInfo : mRouteChargeProgressViews.keySet()) {
                if (isTheSamePoi(poiInfo, poiInfoEntity)) {
                    addPoiInfo = poiInfo;
                    break;
                }
            }
        }
        if (addPoiInfo == null) {
            Logger.e(TAG, " addPoiInfo is null");
            return;
        }
        RouteParam routeParams = new RouteParam();
        for (RouteParam routeParam : mGasChargeAlongList) {
            if (mRoutePackage.isTheSamePoi(routeParam, addPoiInfo)) {
                routeParams = routeParam;
                break;
            }
        }
        removeRouteChargePoiUi(addPoiInfo);
        final int index = mChargePoiDistanceList.indexOf(addPoiInfo.getSort_distance());
        if (index == -1) {
            Logger.d(TAG, "mChargePoiDistanceList: " + mChargePoiDistanceList + " distance:" + addPoiInfo.getSort_distance());
            return;
        }
        mChargePoiDistanceList.remove(index);
        viewCollision();
        updateExhaustDistance();
        mGasChargeAlongList.remove(routeParams);
        mRoutePackage.setGasChargeAlongList(mGasChargeAlongList);

        updateChargeList();
    }

    /***
     * 设置充电UI
     * @param poiInfoEntity POI数据
     * @param progress 百分比
     */
    public void addRouteChargePoiUi(final PoiInfoEntity poiInfoEntity, final float progress) {
        if (null == mViewBinding.routeChargeProgressIcons) {
            Logger.e(TAG, "mRouteChargeGasListPageView = null");
            return;
        }
        ThreadManager.getInstance().postUi(() -> {
            final SkinConstraintLayout routeChargeProgressLayout = mViewBinding.routeChargeProgressIcons;
            final LayoutInflater inflater = LayoutInflater.from(getContext());
            final View customViewItem = inflater.inflate(R.layout.item_route_charge_progress, routeChargeProgressLayout, false);
            customViewItem.setId(View.generateViewId());
            final SkinTextView distanceText = customViewItem.findViewById(R.id.tv_route_charge);
            distanceText.setVisibility(View.GONE);
            if (poiInfoEntity.getDistance() == null) {
                distanceText.setText(TimeUtils.getInstance().getDistanceString(poiInfoEntity.getSort_distance()));
            } else {
                distanceText.setText(poiInfoEntity.getDistance().replace("公里", "km").replace("米","m"));
            }
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
            PoiInfoEntity savePoiInfo = new PoiInfoEntity();
            savePoiInfo.setPid(poiInfoEntity.getPid());
            savePoiInfo.setSort_distance(poiInfoEntity.getSort_distance());
            savePoiInfo.setName(poiInfoEntity.getName());
            savePoiInfo.setAddress(poiInfoEntity.getAddress());
            savePoiInfo.setDistance(poiInfoEntity.getDistance());
            savePoiInfo.setPoint(poiInfoEntity.getPoint());
            mRouteChargeProgressViews.put(savePoiInfo, customViewItem);
            mChargePoiDistanceList.add(poiInfoEntity.getSort_distance());
            viewCollision();
            updateExhaustDistance();
        });
    }

    /***
     * 移除充电UI
     * @param poiInfoEntity POI数据
     */
    public void removeRouteChargePoiUi(final PoiInfoEntity poiInfoEntity) {
        if (null == mViewBinding.routeChargeProgressIcons) {
            Logger.e(TAG, "routeChargeProgressIcons = null");
            return;
        }
        ThreadManager.getInstance().postUi(() -> {
            if (mRouteChargeProgressViews != null) {
                final View view = mRouteChargeProgressViews.get(poiInfoEntity);
                if (view != null && mViewBinding != null) {
                    mViewBinding.routeChargeProgressIcons.removeView(view);
                }
                mRouteChargeProgressViews.remove(poiInfoEntity);
            }
        });
    }

    /***
     * 清除所有充电UI
     */
    public void clearRouteChargePoiUi() {
        ThreadManager.getInstance().postUi(() -> {
            if (mRouteChargeProgressViews != null && !mRouteChargeProgressViews.isEmpty()) {
                for (PoiInfoEntity poiInfoEntity : mRouteChargeProgressViews.keySet()) {
                    final View view = mRouteChargeProgressViews.get(poiInfoEntity);
                    if (mViewBinding != null && view != null && !ConvertUtils.isEmpty(mViewBinding.routeChargeProgressIcons)) {
                        mViewBinding.routeChargeProgressIcons.removeView(view);
                    }
                    mRouteChargeProgressViews.remove(poiInfoEntity);
                }
            }
            mChargePoiDistanceList.clear();
        });
    }

    public boolean isBelongAlongList(final PoiInfoEntity poiInfoEntity) {
        if (ConvertUtils.isEmpty(poiInfoEntity)) {
            return false;
        }
        if (mGasChargeAlongList == null || mGasChargeAlongList.isEmpty()) {
            return false;
        }
        for (RouteParam routeParam : mGasChargeAlongList) {
            if (mRoutePackage.isTheSamePoi(routeParam, poiInfoEntity)) {
                return true;
            }
        }
        return false;
    }

    /**
     * 判断两个点是否一样
     * @param firstRouteParam  第一参数
     * @param secondRouteParam 第二个参数
     * */
    public boolean isTheSamePoi(final RouteParam firstRouteParam, final RouteParam secondRouteParam) {
        if (ConvertUtils.isEmpty(firstRouteParam) || ConvertUtils.isEmpty(secondRouteParam)
                || ConvertUtils.isEmpty(firstRouteParam.getRealPos())
                || ConvertUtils.isEmpty(secondRouteParam.getRealPos())) {
            return false;
        }
        return (!ConvertUtils.isEmpty(firstRouteParam.getPoiID()) && Objects.equals(firstRouteParam.getPoiID(), secondRouteParam.getPoiID()))
                || (firstRouteParam.getRealPos().getLat() == secondRouteParam.getRealPos().getLat()
                && firstRouteParam.getRealPos().getLon() == secondRouteParam.getRealPos().getLon());
    }

    /**
     * 判断两个点是否一样
     * @param firstRouteParam  第一参数
     * @param secondRouteParam 第二个参数
     * */
    public boolean isTheSamePoi(final PoiInfoEntity firstRouteParam, final PoiInfoEntity secondRouteParam) {
        if (ConvertUtils.isEmpty(firstRouteParam) || ConvertUtils.isEmpty(secondRouteParam)
                || ConvertUtils.isEmpty(firstRouteParam.getPoint())
                || ConvertUtils.isEmpty(secondRouteParam.getPoint())) {
            return false;
        }
        return (!ConvertUtils.isEmpty(firstRouteParam.getPid()) && Objects.equals(firstRouteParam.getPid(), secondRouteParam.getPid()))
                || (firstRouteParam.getPoint().getLat() == secondRouteParam.getPoint().getLat()
                && firstRouteParam.getPoint().getLon() == secondRouteParam.getPoint().getLon());
    }

    /***
     * 获取能量耗尽点信息
     * @param index 路线索引
     * @return 能量耗尽点信息
     */
    public EvRangeOnRouteInfo getRangeOnRouteInfo(final int index) {
        if (index == -1 || mRoutePackage.getEvRangeOnRouteInfos() == null
                || mRoutePackage.getEvRangeOnRouteInfos().isEmpty() || index >= mRoutePackage.getEvRangeOnRouteInfos().size()) {
            Logger.d(TAG, "Index out of bounds ");
            return null;
        }

        return mRoutePackage.getEvRangeOnRouteInfos().get(index);
    }

    /**
     * 获取选中的路线信息
     * @return 路线信息
     * */
    public Integer getCurrentIndex() {
        return mRoutePackage.getSelectRouteIndex().get(MapType.MAIN_SCREEN_MAIN_MAP);
    }

    /***
     * 更新充电列表
     */
    public void updateChargeList() {
        boolean isSame = true;
        if (mGasChargeAlongList.size() != mRouteGasChargeAlongList.size()) {
            isSame = false;
        } else {
            for (RouteParam fristRouteParam : mGasChargeAlongList) {
                boolean hasSame = false;
                for (RouteParam secondRouteParam : mRouteGasChargeAlongList) {
                    if (isTheSamePoi(fristRouteParam, secondRouteParam)) {
                        hasSame = true;
                        break;
                    }
                }
                if (!hasSame) {
                    isSame = false;
                    break;
                }
            }
        }
        if (mViewBinding.routeChargeListAlongWaySure == null || mViewBinding.routeChargeListAlongWayCancel == null) {
            return;
        }
        if (isSame) {
            mViewBinding.routeChargeListAlongWaySure.setVisibility(View.GONE);
            mViewBinding.routeChargeListAlongWayCancel.setVisibility(View.GONE);
        } else {
            if (mIsFilterViewShow) {
                return;
            }
            mViewBinding.routeChargeListAlongWaySure.setVisibility(View.VISIBLE);
            mViewBinding.routeChargeListAlongWayCancel.setVisibility(View.VISIBLE);
        }
    }

    public void addRemoveClick(final int position, final PoiInfoEntity poiInfoEntity) {
        if ((mSearchType == AutoMapConstant.SearchType.ALONG_WAY_SEARCH
                || mSearchType == AutoMapConstant.SearchType.EN_ROUTE_KEYWORD_SEARCH) && mRouteAround) {
            routeClickEvent(poiInfoEntity, position);
            return;
        }
        if (mRoutePackage.isStartOrEndRouteParam(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity)) {
            ToastUtils.Companion.getInstance().showCustomToastView(
                    ResourceUtils.Companion.getInstance().getString(R.string.route_error_add_start_end));
            return;
        }
        if (mRoutePackage.isBelongRouteParam(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity)) {
            mRoutePackage.removeVia(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity, true);
        } else {
            mRoutePackage.addViaPoint(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity);
        }
    }

    public void onSearchItemClick(int index) {
        List<PoiInfoEntity> poiInfoEntities = mSearchResultEntity.getPoiList();
        if (poiInfoEntities != null && !poiInfoEntities.isEmpty() && index < poiInfoEntities.size() && index != -1) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"index: "+index);
            if(!ConvertUtils.isNull(mScreenViewModel.getCurrentFragment()) && mScreenViewModel.getCurrentFragment().getClass().getName().contains("PoiDetailsFragment")){
                closeCurrentFragment();
            }
            mViewBinding.recyclerSearchResult.post(new Runnable() {
                @Override
                public void run() {
                    addRemoveClick(index, poiInfoEntities.get(index));
                    LinearLayoutManager layoutManager = (LinearLayoutManager) mViewBinding.recyclerSearchResult.getLayoutManager();
                    if(!ConvertUtils.isNull(layoutManager)){
                        layoutManager.scrollToPositionWithOffset(index,0);
                    }
                }
            });
        }
    }


    public void viewCollision() {
        if (mRouteChargeProgressViews == null || mRouteChargeProgressViews.isEmpty()
                || mChargePoiDistanceList == null || mChargePoiDistanceList.isEmpty()) {
            Logger.e(TAG, "viewCollision is null");
            return;
        }
        ThreadManager.getInstance().postUi(() -> {
            List<Integer> viewCollisionList = new ArrayList<>(mChargePoiDistanceList);
            final float displayProgress = ((float) mViewWidth / mTotalWidth) * mRouteTotalDistance;
            float totalDisplayProgress = mRouteTotalDistance;
            if (mEndWidth == 0) {
                mEndWidth = mViewWidth + mViewWidth/2;
            }
            if (mTotalWidth - mEndWidth > 0) {
                totalDisplayProgress  = ((float) (mTotalWidth - mEndWidth)/ mTotalWidth) * mRouteTotalDistance;
            }

            for (Map.Entry<PoiInfoEntity, View> entry : mRouteChargeProgressViews.entrySet()) {
                if (entry.getKey() != null && entry.getValue() != null) {
                    final SkinTextView distanceText = entry.getValue().findViewById(R.id.tv_route_charge);
                    int distance = entry.getKey().getSort_distance();
                    int index = viewCollisionList.indexOf(distance);
                    if (distanceText == null || index == -1) {
                        Logger.e(TAG, "distanceText view is null");
                        continue;
                    }
                    if (distance > totalDisplayProgress) {
                        distanceText.setVisibility(View.GONE);
                        viewCollisionList.remove(index);
                        continue;
                    }
                    boolean isShow = true;
                    for (int i = 0; i < viewCollisionList.size(); i++) {
                        if (i == index) {
                            continue;
                        }
                        Integer distanceValue = viewCollisionList.get(i);
                        if (distanceValue != null
                                && distanceValue > distance - displayProgress
                                && distanceValue < distance + displayProgress) {
                            isShow = false;
                            distanceText.setVisibility(View.GONE);
                            viewCollisionList.remove(index);
                            break;
                        }
                    }
                    if (isShow) {
                        distanceText.setVisibility(View.VISIBLE);
                    }
                }
            }
        });
    }


    //------------算路沿途搜***************************************************/

    public void showLoading(final boolean isShow){
        if(ConvertUtils.isNull(mViewBinding)){
            return;
        }
        mViewBinding.ivLoading.setVisibility(isShow ? View.VISIBLE : View.GONE);
        mViewBinding.ilLoading.setVisibility(isShow ? View.VISIBLE : View.GONE);
        mViewBinding.searchLabelFilter.setVisibility(isShow ? GONE : View.VISIBLE);
        mViewBinding.searchResultNoData.setVisibility(isShow ? GONE : View.VISIBLE);
        mViewBinding.recyclerSearchResult.setVisibility(isShow ? GONE : View.VISIBLE);
        mViewBinding.pullRefreshLayout.setVisibility(isShow ? GONE : View.VISIBLE);
        if (!ConvertUtils.isNull(mAnimator)) {
            if (isShow && !mAnimator.isRunning()) {
                mAnimator.start();
            } else {
                mAnimator.cancel();
            }
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

    public void updateSearchEntity() {
        if (!ConvertUtils.isEmpty(mGasChargeAlongList) && !ConvertUtils.isEmpty(mResultEntity)) {
            for (int i = 0; i < mGasChargeAlongList.size(); i++) {
                for (int j = 0; j < mResultEntity.getPoiList().size(); j++) {
                    if (Objects.equals(mGasChargeAlongList.get(i).getPoiID(), mResultEntity.getPoiList().get(j).getPid())) {
                        mResultEntity.getPoiList().get(j).setMIsVisible(true);
                    }
                }
            }
        }
    }

    public void onBackPressed() {
        if (null == mScreenViewModel) return;
        if (mIsOpenFromNavi) {
            mScreenViewModel.closeSearchOpenFromNavi();
        } else {
            mScreenViewModel.closeSearch();
        }
    }
}