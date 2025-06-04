package com.fy.navi.scene.ui.search;


import android.content.Context;
import android.text.Editable;
import android.text.TextWatcher;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;
import android.view.inputmethod.InputMethodManager;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.alibaba.android.arouter.launcher.ARouter;
import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.fy.navi.burypoint.anno.HookMethod;
import com.fy.navi.burypoint.bean.BuryProperty;
import com.fy.navi.burypoint.constant.BuryConstant;
import com.fy.navi.burypoint.controller.BuryPointController;
import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.R;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.scene.api.search.IClearEditTextListener;
import com.fy.navi.scene.databinding.SugSearchResultViewBinding;
import com.fy.navi.scene.impl.search.SceneSugSearchPoiListImpl;
import com.fy.navi.scene.impl.search.SearchFragmentFactory;
import com.fy.navi.scene.ui.adapter.SearchHistoryAdapter;
import com.fy.navi.scene.ui.adapter.SearchResultAdapter;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.route.RoutePoiType;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.greendao.history.History;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.ui.base.BaseFragment;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * @Description: 联想搜索结果列表 scene
 * @Date: 2020/4/16 11:05 AM
 * @CreateDate: $ $
 */
public class SceneSugSearchPoiList extends BaseSceneView<SugSearchResultViewBinding, SceneSugSearchPoiListImpl> {
    private SearchResultAdapter mAdapter;
    private IClearEditTextListener mClearEditTextListener;
    private SearchHistoryAdapter mSearchHistoryAdapter;

    public SceneSugSearchPoiList(@NonNull final Context context) {
        super(context);
    }

    public SceneSugSearchPoiList(@NonNull final Context context, @Nullable final AttributeSet attrs) {
        super(context, attrs);

    }

    public SceneSugSearchPoiList(@NonNull final Context context, @Nullable final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);

    }

    @Override
    protected SugSearchResultViewBinding createViewBinding(final LayoutInflater inflater, final ViewGroup viewGroup) {
        return SugSearchResultViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneSugSearchPoiListImpl initSceneImpl() {
        return new SceneSugSearchPoiListImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setISceneSugSearchPoiList(mScreenViewModel);
    }

    @Override
    protected void initObserver() {
        setupRecyclerView();
        setupHistoryRecycleView();
        setupSearchActions();
//        requestFocusAndShowKeyboard();
    }

    /**
     * 初始化 RecyclerView
     */
    private void setupRecyclerView() {
        final LinearLayoutManager layoutManager = new LinearLayoutManager(getContext());
        layoutManager.setOrientation(LinearLayoutManager.VERTICAL);
        mViewBinding.recyclerSearchResult.setLayoutManager(layoutManager);

        mAdapter = new SearchResultAdapter();
        mViewBinding.recyclerSearchResult.setAdapter(mAdapter);
        mViewBinding.recyclerSearchResult.addOnScrollListener(new RecyclerView.OnScrollListener() {
            @Override
            public void onScrolled(@NonNull RecyclerView recyclerView, final int dx, final int dy) {
                super.onScrolled(recyclerView, dx, dy);
                if (dy != 0) {
                    //列表滑动时，隐藏软键盘
                    hideInput();
                }
            }

            @Override
            public void onScrollStateChanged(@NonNull final RecyclerView recyclerView, final int newState) {
                super.onScrollStateChanged(recyclerView, newState);
                if (newState == RecyclerView.SCROLL_STATE_DRAGGING) {
                    // 用户正在拖动 RecyclerView,隐藏软键盘
                    hideInput();
                }
            }
        });
        mAdapter.setOnItemClickListener(new SearchResultAdapter.OnItemClickListener() {
            @Override
            public void onItemClick(final int position, final PoiInfoEntity poiInfoEntity) {
                sendBuryPointForHistoryClick(poiInfoEntity.getName());
                final Fragment fragment = (Fragment) ARouter.getInstance()
                        .build(RoutePath.Search.POI_DETAILS_FRAGMENT)
                        .navigation();
                addFragment((BaseFragment) fragment, SearchFragmentFactory.createPoiDetailsFragment(
                        AutoMapConstant.SourceFragment.SUG_SEARCH_FRAGMENT, AutoMapConstant.PoiType.POI_KEYWORD, poiInfoEntity));
                hideInput();
            }

            @Override
            public void onNaviClick(final int position, final PoiInfoEntity poiInfoEntity) {
                if (SearchPackage.getInstance().isAlongWaySearch()) {
                    RoutePackage.getInstance().addViaPoint(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity);
                } else {
                    final Fragment fragment = (Fragment) ARouter.getInstance()
                            .build(RoutePath.Route.ROUTE_FRAGMENT)
                            .navigation();
                    addFragment((BaseFragment) fragment, SearchFragmentFactory.createRouteFragment(poiInfoEntity));
                }
                hideInput();
            }
        });
    }

    public void setupHistoryRecycleView(){
        final LinearLayoutManager layoutManager = new LinearLayoutManager(getContext());
        layoutManager.setOrientation(LinearLayoutManager.VERTICAL);
        mViewBinding.recyclerSearchHistory.setLayoutManager(layoutManager);

        mSearchHistoryAdapter = new SearchHistoryAdapter();
        mSearchHistoryAdapter.setNoShowActionContainer();
        mViewBinding.recyclerSearchHistory.setAdapter(mSearchHistoryAdapter);
        final List<History> list = getSearchKeywordRecord();
        if(list.isEmpty()){
            mViewBinding.recyclerSearchHistory.setVisibility(GONE);
            mViewBinding.recyclerNoHint.setVisibility(VISIBLE);
            mViewBinding.recyclerNoHint.setText(ResourceUtils.Companion.getInstance().getString(R.string.shv_record_null));
        }else if(!list.isEmpty() && getEditText().isEmpty()){
            mViewBinding.recyclerSearchHistory.setVisibility(VISIBLE);
            mViewBinding.recyclerNoHint.setVisibility(GONE);
            mSearchHistoryAdapter.notifyList(list);
            mSearchHistoryAdapter.setMIsShowIndex(false);
        }
        mViewBinding.recyclerSearchHistory.addOnScrollListener(new RecyclerView.OnScrollListener() {
            @Override
            public void onScrolled(@NonNull final RecyclerView recyclerView, final int dx, final int dy) {
                super.onScrolled(recyclerView, dx, dy);
                if (dy != 0) {
                    //列表滑动时，隐藏软键盘
                    hideInput();
                }
            }

            @Override
            public void onScrollStateChanged(@NonNull final RecyclerView recyclerView, final int newState) {
                super.onScrollStateChanged(recyclerView, newState);
                if (newState == RecyclerView.SCROLL_STATE_DRAGGING) {
                    // 用户正在拖动 RecyclerView,隐藏软键盘
                    hideInput();
                }
            }
        });
        mSearchHistoryAdapter.setOnItemClickListener(new SearchHistoryAdapter.ItemClickListener() {
            @Override
            public void onItemClick(final int position, final History history) {
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "BP====onItemClick position:" + position + " history:" + history);
                if (history.getMType() == AutoMapConstant.SearchKeywordRecordKey.SEARCH_KEYWORD_RECORD_KEY) {
                    final Fragment fragment = (Fragment) ARouter.getInstance()
                            .build(RoutePath.Search.SEARCH_RESULT_FRAGMENT)
                            .navigation();

                    addFragment((BaseFragment) fragment,
                            SearchFragmentFactory.createKeywordFragment(
                                    AutoMapConstant.SourceFragment.MAIN_SEARCH_FRAGMENT,
                                    AutoMapConstant.SearchType.SEARCH_KEYWORD,
                                    history.getMKeyWord(),
                                    null));

                } else {
                    final GeoPoint historyPoint = parseGeoPoint(history.getMEndPoint());
                    final PoiInfoEntity poiInfoEntity = new PoiInfoEntity()
                            .setPid(history.getMPoiId())
                            .setPoint(historyPoint);
                    final Fragment fragment = (Fragment) ARouter.getInstance()
                            .build(RoutePath.Search.POI_DETAILS_FRAGMENT)
                            .navigation();
                    addFragment((BaseFragment) fragment, SearchFragmentFactory.createPoiDetailsFragment(
                            AutoMapConstant.SourceFragment.MAIN_SEARCH_FRAGMENT, AutoMapConstant.PoiType.POI_KEYWORD, poiInfoEntity));
                }
                hideInput();
            }

            @Override
            public void onNaviClick(final int position, final History history) {
                final PoiInfoEntity poiInfoEntity = new PoiInfoEntity();
                poiInfoEntity.setName(history.getMEndPoiName());
                poiInfoEntity.setAddress(history.getMEndPoiName());
                poiInfoEntity.setPoiType(RoutePoiType.ROUTE_POI_TYPE_END);
                poiInfoEntity.setPid(history.getMPoiId());
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onNaviClick: historyPoint = " + history.getMEndPoint());

                final GeoPoint historyPoint = parseGeoPoint(history.getMEndPoint());
                final GeoPoint geoPoint = new GeoPoint();
                geoPoint.setLon(historyPoint.getLon());
                geoPoint.setLat(historyPoint.getLat());
                poiInfoEntity.setPoint(geoPoint);
                if (SearchPackage.getInstance().isAlongWaySearch()) {
                    RoutePackage.getInstance().addViaPoint(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity);
                } else {
                    final Fragment fragment = (Fragment) ARouter.getInstance()
                            .build(RoutePath.Route.ROUTE_FRAGMENT)
                            .navigation();
                    addFragment((BaseFragment) fragment, SearchFragmentFactory.createRouteFragment(poiInfoEntity));
                }
                hideInput();
            }
        });

    }

    /**
     * 搜索相关事件
     */
    private void setupSearchActions() {
        mViewBinding.sclSearchTopView.ivClose.setOnClickListener(v -> mScreenViewModel.closeSearch());
        mViewBinding.sclSearchTopView.searchBarEditView.setHint(ResourceUtils.Companion.getInstance().getString(R.string.main_search_hint));
        mViewBinding.sclSearchTopView.searchBarEditView.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(final CharSequence s, final int start, final int count, final int after) {

            }

            @Override
            public void onTextChanged(final CharSequence s, final int start, final int before, final int count) {
            }

            @Override
            public void afterTextChanged(final Editable editable) {
                if (mClearEditTextListener != null) {
                    mClearEditTextListener.onEditTextChanged(editable.toString().trim());
                }
                if (!editable.toString().trim().isEmpty()) {
                    mViewBinding.sclSearchTopView.ivEditClear.setVisibility(VISIBLE);
                    mViewBinding.recyclerSearchResult.setVisibility(VISIBLE);
                    mViewBinding.recyclerSearchHistory.setVisibility(GONE);
                    suggestionSearch(editable.toString().trim());
                } else {
                    Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"Text is empty");
                    mViewBinding.sclSearchTopView.ivEditClear.setVisibility(GONE);
                    mViewBinding.recyclerSearchResult.setVisibility(GONE);
                    mViewBinding.recyclerSearchHistory.setVisibility(VISIBLE);
                    mAdapter.clearList();
                    final List<History> list = getSearchKeywordRecord();
                    if(list.isEmpty()){
                        mViewBinding.recyclerSearchHistory.setVisibility(GONE);
                        mViewBinding.recyclerNoHint.setVisibility(VISIBLE);
                        mViewBinding.recyclerNoHint.setText(ResourceUtils.Companion.getInstance().getString(R.string.shv_record_null));
                    }else{
                        mViewBinding.recyclerSearchHistory.setVisibility(VISIBLE);
                        mViewBinding.recyclerNoHint.setVisibility(GONE);
                        mSearchHistoryAdapter.notifyList(list);
                        mSearchHistoryAdapter.setMIsShowIndex(false);
                    }
                }
            }
        });

        mViewBinding.sclSearchTopView.searchBarEditView.setOnEditorActionListener((v, actionId, event) -> {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onEditorActionListener actionId: " + actionId);
            // 预搜索界面逻辑处理，跳转到搜索结果页面
            final Fragment fragment = (Fragment) ARouter.getInstance()
                    .build(RoutePath.Search.SEARCH_RESULT_FRAGMENT)
                    .navigation();
            addFragment((BaseFragment) fragment, SearchFragmentFactory.createKeywordFragment(
                    AutoMapConstant.SourceFragment.SUG_SEARCH_FRAGMENT, AutoMapConstant.SearchType.SEARCH_KEYWORD, getEditText(), null));
            hideInput();
            return true;
        });

        mViewBinding.sclSearchTopView.ivEditClear.setOnClickListener(view -> clearEditText());
    }

    /**
     * 执行预搜索
     * @param keyword 搜索关键字
     */
    public void suggestionSearch(final String keyword) {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "sugSearch search: " + ", Keyword: " + keyword);
        mScreenViewModel.suggestionSearch(keyword);
    }

    /**
     * 更新搜索结果
     * @param searchResultEntity 搜索结果实体类
     * @param isRestore 是否是切换日夜模式导致的更新回调
     */
    public void notifySearchResult(final SearchResultEntity searchResultEntity, final boolean isRestore) {
        if ((searchResultEntity == null || searchResultEntity.getPoiList().isEmpty()) && !getEditText().isEmpty()) {
            ToastUtils.Companion.getInstance().showCustomToastView("暂无数据");
            mViewBinding.recyclerSearchResult.setVisibility(GONE);
            mViewBinding.recyclerNoHint.setVisibility(VISIBLE);
            mViewBinding.recyclerNoHint.setText(ResourceUtils.Companion.getInstance().getString(R.string.sug_search_result_no_data));
            mAdapter.clearList();
            return;
        }
        if (searchResultEntity != null && !ConvertUtils.isEmpty(searchResultEntity.getKeyword())) {
            if ((ConvertUtils.isEmpty(getEditText()) || !ConvertUtils.equals(getEditText(), searchResultEntity.getKeyword()))
                    && isRestore) {
                mViewBinding.sclSearchTopView.searchBarEditView.setText(searchResultEntity.getKeyword());
            }
        }
        if (mAdapter != null && !getEditText().isEmpty()) {
            mViewBinding.recyclerSearchResult.setVisibility(VISIBLE);
            mViewBinding.recyclerNoHint.setVisibility(GONE);
            mAdapter.notifyList(searchResultEntity);
        }
    }

    /**
     * 隐藏软键盘
     */
    public void hideInput() {
        final InputMethodManager imm = (InputMethodManager) getContext().getSystemService(Context.INPUT_METHOD_SERVICE);
        if (imm != null) {
            imm.hideSoftInputFromWindow(getWindowToken(), 0);
        }
    }

    /**
     * 获取输入框内容
     *
     * @return 输入框内容
     */
    private String getEditText() {
        return mViewBinding.sclSearchTopView.searchBarEditView.getText() != null ?
                mViewBinding.sclSearchTopView.searchBarEditView.getText().toString().trim() : "";
    }

    /**
     * 清空输入框内容
     */
    public void clearEditText() {
        mViewBinding.sclSearchTopView.searchBarEditView.setText("");
        if (null != mAdapter) {
            mAdapter.clearList();
        }
    }

    /**
     * 请求焦点并显示软键盘
     */
    public void requestFocusAndShowKeyboard() {
        // 确保视图已经附加到窗口
        mViewBinding.sclSearchTopView.searchBarEditView.post(() -> {
            mViewBinding.sclSearchTopView.searchBarEditView.requestFocus();
            showKeyboard();
        });
    }

    /**
     * 显示软键盘
     */
    private void showKeyboard() {
        final InputMethodManager imm = (InputMethodManager) getContext().getSystemService(Context.INPUT_METHOD_SERVICE);
        if (imm != null) {
            imm.showSoftInput(mViewBinding.sclSearchTopView.searchBarEditView, InputMethodManager.SHOW_IMPLICIT);
        }
    }

    @HookMethod(eventName = BuryConstant.EventName.AMAP_DESTINATION_HISTORY_SELECT)
    private void sendBuryPointForHistoryClick(String value) {
        BuryProperty buryProperty = new BuryProperty.Builder()
                .setParams(BuryConstant.ProperType.BURY_KEY_SEARCH_CONTENTS, value)
                .build();
        BuryPointController.getInstance().setBuryProps(buryProperty);
    }

    public List<History> getSearchKeywordRecord(){
        return mScreenViewModel.getSearchKeywordRecord();
    }

    /**
     * 解析geoPoint字符串
     * @param geoPointString geoPoint字符串
     * @return GeoPoint
     */
    private GeoPoint parseGeoPoint(final String geoPointString) {
        final Pattern pattern = Pattern.compile("lon=([-\\d.]+), lat=([-\\d.]+)");
        final Matcher matcher = pattern.matcher(geoPointString);

        double lon = 0.0;
        double lat = 0.0;

        if (matcher.find()) {
            lon = Double.parseDouble(matcher.group(1));
            lat = Double.parseDouble(matcher.group(2));
        } else {
            Logger.e(MapDefaultFinalTag.SEARCH_HMI_TAG, "parseGeoPoint: No match found for GeoPoint string: " + geoPointString);
        }
        return new GeoPoint(lon, lat);
    }

    public void setEditTextChangedListener(final IClearEditTextListener clickListener) {
        this.mClearEditTextListener = clickListener;
    }
}