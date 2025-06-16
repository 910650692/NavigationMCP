package com.fy.navi.scene.ui.search;


import android.annotation.SuppressLint;
import android.content.Context;
import android.os.Bundle;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.alibaba.android.arouter.launcher.ARouter;
import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.burypoint.bean.BuryProperty;
import com.fy.navi.burypoint.constant.BuryConstant;
import com.fy.navi.burypoint.controller.BuryPointController;
import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.R;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.scene.adapter.GridSpacingItemDecoration;
import com.fy.navi.scene.adapter.HorizontalSpaceItemDecoration;
import com.fy.navi.scene.api.route.ISceneRouteGasStationChargeSelectCallBack;
import com.fy.navi.scene.api.search.IOnFilterItemClickListener;
import com.fy.navi.scene.databinding.PoiSearchResultViewBinding;
import com.fy.navi.scene.impl.search.SceneSearchPoiListImpl;
import com.fy.navi.scene.impl.search.SearchFragmentFactory;
import com.fy.navi.scene.ui.adapter.FilterChildListAdapter;
import com.fy.navi.scene.ui.adapter.FilterListAdapter;
import com.fy.navi.scene.ui.adapter.QuickFilterListAdapter;
import com.fy.navi.scene.ui.adapter.SearchResultAdapter;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.mapdata.ProvDataInfo;
import com.fy.navi.service.define.search.FavoriteInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchCategoryLocalInfo;
import com.fy.navi.service.define.search.SearchChildCategoryLocalInfo;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.setting.SettingUpdateObservable;
import com.fy.navi.service.logicpaket.user.behavior.BehaviorPackage;
import com.fy.navi.ui.base.BaseFragment;
import com.fy.navi.ui.view.refresh.RefreshListener;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

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
    private FilterChildListAdapter mFilterOneChildAdapter;
    private FilterChildListAdapter mFilterTwoChildAdapter;
    private FilterChildListAdapter mFilterThreeChildAdapter;
    private QuickFilterListAdapter mQuickFilterListAdapter;
    private int maxPageNum = 1;
    private int mPageNum = 1;
    private SearchLoadingDialog mSearchLoadingDialog;
    private int mSearchType = AutoMapConstant.SearchType.SEARCH_KEYWORD;
    private String mSearchText;
    private PoiInfoEntity mPoiInfoEntity;
    private SearchResultEntity mResultEntity;
    private List<SearchCategoryLocalInfo> mLocalInfoList;
    private boolean mIsFilterViewShow = false;
    private final int mHorizontalSpacing = 12;
    private final int mQuickLabelHorizontalSpacing = 14;
    private final int mChildHorizontalSpacing = 16;
    private final int mChildVerticalSpacing = 16;
    private final int mSpanCount = 3;
    //第一个一级菜单当前正在被选中的二级菜单下标
    private int mCurrentSelectedIndex1 = -1;
    //第二个一级菜单当前正在被选中的二级菜单下标
    private int mCurrentSelectedIndex2 = -1;
    //第三个一级菜单当前正在被选中的二级菜单下标
    private int mCurrentSelectedIndex3 = -1;
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
    private List<SearchChildCategoryLocalInfo> mChildQuickList;

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
        mSearchLoadingDialog = new SearchLoadingDialog(getContext());
        mCurrentSelectedQuick = -1;
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

        final RecyclerView.ItemDecoration gridDecoration = new GridSpacingItemDecoration(getContext(),
                mSpanCount, mChildVerticalSpacing, mChildHorizontalSpacing, false);
        mViewBinding.searchFilterView.searchFilterList1Child.setLayoutManager(new GridLayoutManager(getContext(), mSpanCount));
        mFilterOneChildAdapter = new FilterChildListAdapter();
        mViewBinding.searchFilterView.searchFilterList1Child.setAdapter(mFilterOneChildAdapter);
        mViewBinding.searchFilterView.searchFilterList1Child.addItemDecoration(gridDecoration);

        mViewBinding.searchFilterView.searchFilterList2Child.setLayoutManager(new GridLayoutManager(getContext(), mSpanCount));
        mFilterTwoChildAdapter = new FilterChildListAdapter();
        mViewBinding.searchFilterView.searchFilterList2Child.setAdapter(mFilterTwoChildAdapter);
        mViewBinding.searchFilterView.searchFilterList2Child.addItemDecoration(gridDecoration);

        mViewBinding.searchFilterView.searchFilterList3Child.setLayoutManager(new GridLayoutManager(getContext(), mSpanCount));
        mFilterThreeChildAdapter = new FilterChildListAdapter();
        mViewBinding.searchFilterView.searchFilterList3Child.setAdapter(mFilterThreeChildAdapter);
        mViewBinding.searchFilterView.searchFilterList3Child.addItemDecoration(gridDecoration);

        // 快筛adapter
        final RecyclerView.ItemDecoration quickItemDecoration = new HorizontalSpaceItemDecoration(mQuickLabelHorizontalSpacing);
        mViewBinding.searchLabelFilter.setLayoutManager(new LinearLayoutManager(getContext(), RecyclerView.HORIZONTAL, false));
        mQuickFilterListAdapter = new QuickFilterListAdapter();
        mViewBinding.searchLabelFilter.setAdapter(mQuickFilterListAdapter);
        mViewBinding.searchLabelFilter.addItemDecoration(quickItemDecoration);

        mAdapter.setOnItemClickListener(new SearchResultAdapter.OnItemClickListener() {
            @Override
            public void onItemClick(final int position, final PoiInfoEntity poiInfoEntity) {
                sendBuryPointForListSelect(position+1, poiInfoEntity.getName());
                final Fragment fragment = (Fragment) ARouter.getInstance().build(RoutePath.Search.POI_DETAILS_FRAGMENT).navigation();
                if (!ConvertUtils.isEmpty(mScreenViewModel) && !ConvertUtils.isEmpty(mResultEntity)) {
                    mScreenViewModel.setSelectIndex(poiInfoEntity, position, mSearchType);
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
                        if (RoutePackage.getInstance().isBelongRouteParam(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity)) {
                            RoutePackage.getInstance().removeVia(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity, true);
                        } else {
                            RoutePackage.getInstance().addViaPoint(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity);
                        }

                    } else {
                        if (mIsEnd) {
                            RoutePackage.getInstance().requestChangeEnd(mMapTypeId, poiInfoEntity);
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
        // 获取第一个可见item的位置
        if (linearLayoutManager != null) {
            final int firstVisiblePosition = linearLayoutManager.findFirstVisibleItemPosition();
            // 获取最后一个可见item的位置
            final int lastVisiblePosition = linearLayoutManager.findLastVisibleItemPosition();

            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "第一个可见位置: " + firstVisiblePosition);
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "最后一个可见位置: " + lastVisiblePosition);
            if (mAdapter == null) {
                return;
            }
            final List<PoiInfoEntity> poiInfoEntities = mResultEntity.getPoiList();
            if (firstVisiblePosition < 0 || firstVisiblePosition >= mAdapter.getItemCount()
                    || lastVisiblePosition < 0 || lastVisiblePosition >= mAdapter.getItemCount()
                    || firstVisiblePosition >= lastVisiblePosition) {
                if (!ConvertUtils.isEmpty(poiInfoEntities)) {
                    //首次进入可见位置均是-1，默认选中下标0-2的item
                    final int size = Math.min(3, poiInfoEntities.size());
                    for (int i = 0; i < poiInfoEntities.size(); i++) {
                        final PoiInfoEntity poiInfoEntity = poiInfoEntities.get(i);
                        poiInfoEntity.setMIsVisible(i <= size - 1);
                    }
                    mScreenViewModel.updatePoiMarker(poiInfoEntities, 0);
                }
                return;
            }
            if (!ConvertUtils.isEmpty(poiInfoEntities)) {
                // 遍历所有可见的item
                for (int i = 0; i < poiInfoEntities.size(); i++) {
                    if (!ConvertUtils.isEmpty(poiInfoEntities)) {
                        final PoiInfoEntity poiInfoEntity = poiInfoEntities.get(i);
                        poiInfoEntity.setMIsVisible(i >= firstVisiblePosition && i <= lastVisiblePosition);
                    }
                }
                mScreenViewModel.updatePoiMarker(poiInfoEntities, 0);
            }
        }

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
            if (mIsOpenFromNavi) {
                mScreenViewModel.closeSearchOpenFromNavi();
            } else {
                mScreenViewModel.closeSearch();
            }
        });
        mViewBinding.searchTextBarView.searchBarTextView.setOnClickListener(v -> {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "setupSearchActions: " + mSearchType);
            if (mSearchType == AutoMapConstant.SearchType.SEARCH_KEYWORD) {
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
        mViewBinding.searchFilterView.searchFilterConfirm.setOnClickListener(v -> {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "click confirm: ");
            mCurrentSelectedQuick = -1;
            mViewBinding.searchFilterView.searchFilterRoot.setVisibility(GONE);
            mViewBinding.pullRefreshLayout.setVisibility(VISIBLE);
            mIsFilterViewShow = false;
            mViewBinding.searchTextBarView.searchBarTextView.setText(mSearchText);
            mScreenViewModel.keywordSearch(mPageNum, mSearchText, mResultEntity.getRetain(), getClassifyData(), false);
        });
        mViewBinding.searchFilterView.searchFilterCancel.setOnClickListener(v -> {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "click reset: ");
            mViewBinding.searchFilterView.searchFilterRoot.setVisibility(GONE);
            mViewBinding.pullRefreshLayout.setVisibility(VISIBLE);
            mIsFilterViewShow = false;
            mViewBinding.searchTextBarView.searchBarTextView.setText(mSearchText);
            mScreenViewModel.keywordSearch(mPageNum, mSearchText);
        });
        mViewBinding.searchTextBarView.csFilter.setOnClickListener(v -> {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "click filter: ");
            if(mIsChargeSelf){
                ToastUtils.Companion.getInstance().showCustomToastView(getContext().getString(R.string.search_charge_self_filter));
                return;
            }
            if (!mIsFilterViewShow) {
                mViewBinding.searchFilterView.searchFilterRoot.setVisibility(VISIBLE);
                mViewBinding.pullRefreshLayout.setVisibility(GONE);
                mViewBinding.searchResultNoData.setVisibility(GONE);
                mViewBinding.searchLabelFilter.setVisibility(GONE);
                mIsFilterViewShow = true;
                if (null != mResultEntity) {
                    mViewBinding.searchTextBarView.searchBarTextView.setText(getContext().getString(
                            R.string.filter_result, mSearchText, mResultEntity.getTotal()));
                }
            }
            if (!mLocalInfoList.isEmpty()) {
                for (int i = 0; i < mLocalInfoList.size(); i++) {
                    final SearchCategoryLocalInfo searchCategoryLocalInfo = mLocalInfoList.get(i);
                    if (i == 0) {
                        if (ConvertUtils.isEmpty(searchCategoryLocalInfo.getCategoryLocalInfos())) {
                            mViewBinding.searchFilterView.searchFilterTitle1.setVisibility(GONE);
                            mViewBinding.searchFilterView.searchFilterList1.setVisibility(GONE);
                        }
                        mViewBinding.searchFilterView.searchFilterTitle1.setText(searchCategoryLocalInfo.getName());
                        mFilterOneAdapter.setMIsExpand(false);
                        mFilterOneAdapter.setMCurrentExpandName("");
                        mFilterOneAdapter.setCategoryList(searchCategoryLocalInfo.getCategoryLocalInfos());
                        mFilterOneChildAdapter.setCategoryList(null);
                    } else if (i == 1) {
                        if (ConvertUtils.isEmpty(searchCategoryLocalInfo.getCategoryLocalInfos())) {
                            mViewBinding.searchFilterView.searchFilterTitle2.setVisibility(GONE);
                            mViewBinding.searchFilterView.searchFilterList2.setVisibility(GONE);
                        }
                        mViewBinding.searchFilterView.searchFilterTitle2.setText(searchCategoryLocalInfo.getName());
                        mFilterTwoAdapter.setMIsExpand(false);
                        mFilterTwoAdapter.setMCurrentExpandName("");
                        mFilterTwoAdapter.setCategoryList(searchCategoryLocalInfo.getCategoryLocalInfos());
                        mFilterTwoChildAdapter.setCategoryList(null);
                    } else if (i == 2) {
                        if (ConvertUtils.isEmpty(searchCategoryLocalInfo.getCategoryLocalInfos())) {
                            mViewBinding.searchFilterView.searchFilterTitle3.setVisibility(GONE);
                            mViewBinding.searchFilterView.searchFilterList3.setVisibility(GONE);
                        }
                        mViewBinding.searchFilterView.searchFilterTitle3.setText(searchCategoryLocalInfo.getName());
                        mFilterThreeAdapter.setMIsExpand(false);
                        mFilterThreeAdapter.setMCurrentExpandName("");
                        mFilterThreeAdapter.setCategoryList(searchCategoryLocalInfo.getCategoryLocalInfos());
                        mFilterThreeChildAdapter.setCategoryList(null);
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
                mScreenViewModel.keywordSearch(mPageNum, mSearchText, mResultEntity.getRetain(), getClassifyData(), true);
            }

            @Override
            public void onChildListExpandCollapse(final List<SearchChildCategoryLocalInfo> childList, final int position) {
                mCurrentSelectedIndex1 = position;
                refreshLocalInfoListCheckedState(0, mCurrentSelectedIndex1);
                mFilterOneChildAdapter.setCategoryList(childList);
            }
        });
        mFilterTwoAdapter.setFilterItemClickListener(new IOnFilterItemClickListener() {
            @Override
            public void onItemClick(final int position) {
                mCurrentSelectedIndex2 = position;
                refreshLocalInfoListCheckedState(1, mCurrentSelectedIndex2);
                mFilterTwoChildAdapter.setCategoryList(null);
                mScreenViewModel.keywordSearch(mPageNum, mSearchText, mResultEntity.getRetain(), getClassifyData(), true);
            }

            @Override
            public void onChildListExpandCollapse(final List<SearchChildCategoryLocalInfo> childList, final int position) {
                mCurrentSelectedIndex2 = position;
                refreshLocalInfoListCheckedState(1, mCurrentSelectedIndex2);
                mFilterTwoChildAdapter.setCategoryList(childList);
            }
        });
        mFilterThreeAdapter.setFilterItemClickListener(new IOnFilterItemClickListener() {
            @Override
            public void onItemClick(final int position) {
                mCurrentSelectedIndex3 = position;
                refreshLocalInfoListCheckedState(2, mCurrentSelectedIndex3);
                mFilterThreeChildAdapter.setCategoryList(null);
                mScreenViewModel.keywordSearch(mPageNum, mSearchText, mResultEntity.getRetain(), getClassifyData(), true);
            }

            @Override
            public void onChildListExpandCollapse(final List<SearchChildCategoryLocalInfo> childList, final int position) {
                mCurrentSelectedIndex3 = position;
                refreshLocalInfoListCheckedState(2, mCurrentSelectedIndex3);
                mFilterThreeChildAdapter.setCategoryList(childList);
            }
        });
        mQuickFilterListAdapter.setItemClickListener(new QuickFilterListAdapter.OnItemClickListener() {
            @Override
            public void onItemClick(List<SearchChildCategoryLocalInfo> list,int position) {
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"value: "+list + "position: "+position);
                if(position>list.size()) return;
                if (null != mSearchLoadingDialog && mSearchLoadingDialog.isShowing()) {
                    Logger.e(MapDefaultFinalTag.SEARCH_HMI_TAG, "mSearchLoadingDialog is showing");
                } else {
                    mSearchLoadingDialog = new SearchLoadingDialog(getContext());
                    mSearchLoadingDialog.show();
                }
                if(mCurrentSelectedQuick == position){
                    mCurrentSelectedQuick = -1;
                    mScreenViewModel.keywordSearch(mPageNum,mSearchText);
                    return;
                }
                mCurrentSelectedQuick = position;
                SearchChildCategoryLocalInfo info = list.get(position);
                if("charge".equals(info.getValue())){
                    mAdapter.clearList();
                    // 请求SGM自营站数据
                    mScreenViewModel.queryStationNewResult(mResultEntity);
//                    mSearchLoadingDialog.dismiss();
//                    ToastUtils.Companion.getInstance().showCustomToastView(getContext().getString(R.string.search_charge_self_filter_hint));
                }else{
                    Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"value: "+info.getValue());
                    mScreenViewModel.keywordSearchByQuickFilter(mPageNum,mSearchText,mResultEntity.getRetain(),info.getValue(),false);
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
            mScreenViewModel.keywordSearch(mPageNum, mSearchText, mResultEntity.getRetain(), getClassifyData(), true);
        });

        mFilterTwoChildAdapter.setFilterItemClickListener(position -> {
            refreshLocalInfoListCheckedState(1, mCurrentSelectedIndex2);
            mFilterOneAdapter.notifyDataSetChanged();
            mScreenViewModel.keywordSearch(mPageNum, mSearchText, mResultEntity.getRetain(), getClassifyData(), true);
        });

        mFilterThreeChildAdapter.setFilterItemClickListener(position -> {
            refreshLocalInfoListCheckedState(2, mCurrentSelectedIndex3);
            mFilterOneAdapter.notifyDataSetChanged();
            mScreenViewModel.keywordSearch(mPageNum, mSearchText, mResultEntity.getRetain(), getClassifyData(), true);
        });
    }

    /**
     * 刷新本地信息列表选中状态
     *
     * @param index1 二级列表选中下标
     * @param index2 三级列表选中下标
     */
    private void refreshLocalInfoListCheckedState(final int index1, final int index2) {
        if (mLocalInfoList != null && !mLocalInfoList.isEmpty() && index1 < mLocalInfoList.size() - 1) {
            final SearchCategoryLocalInfo categoryLocalInfo = mLocalInfoList.get(index1);
            for (int i = 0; i < categoryLocalInfo.getCategoryLocalInfos().size(); i++) {
                final SearchChildCategoryLocalInfo childInfo = categoryLocalInfo.getCategoryLocalInfos().get(i);
                if (i == index2) {
                    childInfo.setChecked(1);
                } else {
                    childInfo.setChecked(-1);
                    for (SearchChildCategoryLocalInfo child : childInfo.getCategoryLocalInfos()) {
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
                mViewBinding.pullRefreshLayout.finishRefresh();
            }

            @Override
            public void loadMore() {
                if (mPageNum >= maxPageNum || (!ConvertUtils.isEmpty(mSearchResultEntity) && mSearchResultEntity.getPoiList().size() < MAX_LIST_NUMBER)) {
                    Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "没有更多数据了，pageNum: " + mPageNum + " / maxPageNum: " + maxPageNum);
                } else {
                    performSearch(++mPageNum, getEditText(), false);
                }
                mViewBinding.pullRefreshLayout.finishLoadMore();
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
        if (null != mSearchLoadingDialog && mSearchLoadingDialog.isShowing()) {
            Logger.e(MapDefaultFinalTag.SEARCH_HMI_TAG, "mSearchLoadingDialog is showing");
        } else {
            mSearchLoadingDialog = new SearchLoadingDialog(getContext());
            mSearchLoadingDialog.show();
        }
        if (mSearchType == AutoMapConstant.SearchType.SEARCH_KEYWORD
                || mSearchType == AutoMapConstant.SearchType.AROUND_SEARCH) {
            //沿途搜不支持离线，无需切换离线模式重搜
            ThreadManager.getInstance().removeHandleTask(mTimeoutTask);
            ThreadManager.getInstance().postDelay(mTimeoutTask, 6000);
        }
        switch (mSearchType) {
            case AutoMapConstant.SearchType.SEARCH_KEYWORD:
                if (mCityCode != 0) {
                    mScreenViewModel.keywordSearch(pageNum, keyword, mCityCode, false, isReSearch);
                } else {
                    mScreenViewModel.keywordSearch(pageNum, keyword, isReSearch);
                }
                break;
            case AutoMapConstant.SearchType.AROUND_SEARCH:
                mScreenViewModel.aroundSearch(pageNum, keyword, mPoiInfoEntity, String.valueOf(mRange), isReSearch);
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
            } else {
                mScreenViewModel.updatePoiMarker(mResultEntity.getPoiList(), 0);
            }
        }
    }

    /**
     * 更新搜索结果
     * @param taskId 请求任务id
     * @param searchResultEntity 搜索结果实体
     */
    public void notifySearchResult(final int taskId, final SearchResultEntity searchResultEntity) {
        if (mSearchLoadingDialog != null && mSearchLoadingDialog.isShowing()) {
            mSearchLoadingDialog.dismiss();
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
        // 处理用户搜索意图,意图为充电站显示快筛列表
        if(searchResultEntity != null
                && !ConvertUtils.isEmpty(searchResultEntity.getQueryTypeList())
                && isChargeQuery(searchResultEntity.getQueryTypeList())){
            // todo: 功能暂时隐藏
//            mViewBinding.searchLabelFilter.setVisibility(VISIBLE);
//            mChildQuickList = mapCustomLabel(searchResultEntity.getLevel2LocalInfoList(),searchResultEntity);
//            for (int i = 0; i < mChildQuickList.size(); i++) {
//                mChildQuickList.get(i).setChecked(i == mCurrentSelectedQuick ? 1 : 0);
//            }
//            if(!ConvertUtils.isNull(mAdapter) && mCurrentSelectedQuick > -1){
//                mAdapter.setQuickLabel(mChildQuickList.get(mCurrentSelectedQuick).getName());
//            }
//            mQuickFilterListAdapter.setLabelList(mChildQuickList);
        // 自营站返回
        }else if(searchResultEntity != null && searchResultEntity.getIsNetData()){
            // todo: 功能暂时隐藏
//            mViewBinding.searchLabelFilter.setVisibility(VISIBLE);
//            mChildQuickList = mapCustomLabel(new ArrayList<>(),searchResultEntity);
//            for (int i = 0; i < mChildQuickList.size(); i++) {
//                mChildQuickList.get(i).setChecked(i == mCurrentSelectedQuick ? 1 : 0);
//            }
//            if(!ConvertUtils.isNull(mAdapter) && mCurrentSelectedQuick > -1){
//                mAdapter.setQuickLabel(mChildQuickList.get(mCurrentSelectedQuick).getName());
//            }
//            mQuickFilterListAdapter.setLabelList(mChildQuickList);
        }else{
            mViewBinding.searchLabelFilter.setVisibility(GONE);
        }
        if (searchResultEntity == null || searchResultEntity.getPoiList().isEmpty()) {
            ToastUtils.Companion.getInstance().showCustomToastView("抱歉，未找到结果");
            mSearchLoadingDialog.dismiss();
            if (null != mAdapter) {
                mAdapter.clearList();
            }
            if (searchResultEntity != null && searchResultEntity.getPoiType() == 0) {
                //离线搜索无数据时，跳转城市列表搜索界面
                final Fragment fragment = (Fragment) ARouter.getInstance()
                        .build(RoutePath.Search.OFFLINE_SEARCH_FRAGMENT)
                        .navigation();
                addFragment((BaseFragment) fragment, SearchFragmentFactory.createOfflineFragment(mSearchText));
            }
            //搜索无数据时，展示无结果页面
            mViewBinding.searchResultNoData.setVisibility(VISIBLE);
            return;
        }
        //有数据时，隐藏异常提示界面
        mViewBinding.searchResultNoData.setVisibility(GONE);

        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "mSearchType: " + mSearchType);

        if (mScreenViewModel.isAlongWaySearch()) {
            mViewBinding.routeRightTabListChargeScene.setVisibility(VISIBLE);
            if(!ConvertUtils.isEmpty(searchResultEntity.getKeyword())){
                final String queryType = com.android.utils.ResourceUtils.Companion.getInstance().getString(R.string.st_quick_search_charge);
                mViewBinding.routeRightTabListChargeScene.setSearchCharge(queryType.equals(searchResultEntity.getKeyword()));
            }
            mViewBinding.routeRightTabListChargeScene.registerRouteSelectObserver(TAG, this);

        } else {
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
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "城市数据信息: " + cityDataInfo.getName() + "，城市编码: " + mScreenViewModel.getAcCode());
                mViewBinding.searchOfflineHint.setVisibility(VISIBLE);
                mViewBinding.searchOfflineHint.setText(getContext().getString(R.string.search_offline_hint, cityDataInfo.getName()));
            }
        }
        mResultEntity = searchResultEntity;
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "notifySearchResult name: " + searchResultEntity.getKeyword()
                + " Object: " + this.toString());
        mSearchType = searchResultEntity.getSearchType();
        if (!ConvertUtils.isEmpty(searchResultEntity.getPoiList()) && searchResultEntity.getPoiList().size() == 1) {
            //只有一个搜索结果时，直接跳转结果界面
            final Fragment fragment = (Fragment) ARouter.getInstance().build(RoutePath.Search.POI_DETAILS_FRAGMENT).navigation();
            final int poiType = getPoiType(mHomeCompanyType);
            closeCurrentFragment();
            addFragment((BaseFragment) fragment, SearchFragmentFactory.createPoiDetailsFragment(
                    AutoMapConstant.SourceFragment.SEARCH_RESULT_FRAGMENT, poiType, searchResultEntity.getPoiList().get(0)));
            return;
        }
        setMaxPageNum(searchResultEntity.getMaxPageNum());
        if (mAdapter != null) {
            if(!searchResultEntity.getIsNetData()){
                mSearchResultEntity = searchResultEntity;
            }
            mAdapter.notifyList(searchResultEntity);
            //poi批量搜测试代码
//            List<String> pidList = new ArrayList<>();
//            for (PoiInfoEntity poiInfo : searchResultEntity.getMPoiList()) {
//                pidList.add(poiInfo.getPid());
//            }
//            mScreenViewModel.poiListSearch(pidList, 2);
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
        if (!ConvertUtils.isEmpty(mSearchLoadingDialog)) {
            mSearchLoadingDialog.dismiss();
        }
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
        if (!ConvertUtils.isEmpty(mSearchLoadingDialog)) {
            mSearchLoadingDialog.dismiss();
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
            mSearchLoadingDialog.dismiss();
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
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onVoicePoiSort: " + sortValue);
        if (mScreenViewModel != null && mResultEntity != null) {
            mScreenViewModel.keywordSearch(mPageNum, mSearchText, mResultEntity.getRetain(), getVoiceClassifyData(sortValue), false);
        }
    }

    /**
     * 图层点击事件回调
     * @param index 点击下标
     */
    public void onMarkClickCallBack(final int index) {
        if (mResultEntity != null) {
            final List<PoiInfoEntity> list = mResultEntity.getPoiList();
            if (!ConvertUtils.isEmpty(list) && index < list.size()) {
                final Bundle bundle = new Bundle();
                final Fragment fragment = (Fragment) ARouter.getInstance().build(RoutePath.Search.POI_DETAILS_FRAGMENT).navigation();
                bundle.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_DETAIL, list.get(index));
                bundle.putInt(AutoMapConstant.PoiBundleKey.BUNDLE_KEY_START_POI_TYPE, AutoMapConstant.PoiType.POI_KEYWORD);
                addPoiDetailsFragment((BaseFragment) fragment, bundle);
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
        final StringBuilder stringBuilder = new StringBuilder();
        for (SearchCategoryLocalInfo searchCategoryLocalInfo : mLocalInfoList) {
            for (SearchChildCategoryLocalInfo searchChildCategoryLocalInfo : searchCategoryLocalInfo.getCategoryLocalInfos()) {
                if (searchChildCategoryLocalInfo.getChecked() == 1
                        && !ConvertUtils.isEmpty(searchChildCategoryLocalInfo.getValue())) {
                    stringBuilder.append(searchChildCategoryLocalInfo.getValue());
                    stringBuilder.append(APPEND);
                }
                for (SearchChildCategoryLocalInfo searchChildCategoryLocalInfo1 : searchChildCategoryLocalInfo.getCategoryLocalInfos()) {
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
        mViewBinding.routeRightTabListChargeScene.updateUi();
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

    // 判断用户搜索意图是否是充电站
    private boolean isChargeQuery(ArrayList<String> list){
        final String queryType = com.android.utils.ResourceUtils.Companion.getInstance().getString(R.string.st_quick_search_charge);
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
        super.onDestroy();
        ThreadManager.getInstance().removeHandleTask(mTimeoutTask);
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
        if (ConvertUtils.isNull(list)) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "SearchCategoryLocalInfo is empty");
            return new ArrayList<>();
        }
        boolean isPowerType = mScreenViewModel.powerType() == 1;
        boolean isOffline = searchResultEntity.getPoiType() == 0;
        // 添加自营快筛
        if (isPowerType && !isOffline) {
            SearchChildCategoryLocalInfo chargeItem = new SearchChildCategoryLocalInfo()
                    .setName(ResourceUtils.Companion.getInstance().getString(R.string.search_charge_self))
                    .setValue("charge");
            childListByHigh.add(chargeItem);
            childListByLow.add(chargeItem);
        }
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
        // 别克：性价比车型1，凯迪：高品质车型2
        childListByHigh.addAll(ConvertUtils.isEmpty(list) ? new ArrayList<>() : list.get(0).getCategoryLocalInfos());
        childListByLow.addAll(ConvertUtils.isEmpty(list) ? new ArrayList<>() : list.get(0).getCategoryLocalInfos());
        int brand = mScreenViewModel.getBrand();
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"brand: "+brand);
        return brand == 2 ? childListByHigh : childListByLow;
    }
}