package com.fy.navi.scene.ui.search;


import android.content.Context;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.inputmethod.InputMethodManager;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.alibaba.android.arouter.launcher.ARouter;
import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.R;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.scene.adapter.GridSpacingItemDecoration;
import com.fy.navi.scene.databinding.OfflineSearchResultViewBinding;
import com.fy.navi.scene.impl.search.OfflineSearchPoiListImpl;
import com.fy.navi.scene.impl.search.SearchFragmentFactory;
import com.fy.navi.scene.ui.adapter.OfflineCitiesAdapter;
import com.fy.navi.scene.ui.adapter.OfflineProvincesAdapter;
import com.fy.navi.scene.ui.adapter.OfflineSearchResultAdapter;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.mapdata.ProvDataInfo;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.logicpaket.mapdata.MapDataPackage;
import com.fy.navi.ui.base.BaseFragment;

import java.util.ArrayList;
import java.util.List;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * @Description: 联想搜索结果列表 scene
 * @Date: 2020/4/16 11:05 AM
 * @CreateDate: $ $
 */
public class OfflineSearchPoiList extends BaseSceneView<OfflineSearchResultViewBinding, OfflineSearchPoiListImpl> {
    private OfflineProvincesAdapter mAdapter;
    private OfflineSearchResultAdapter mSearchAdapter;
    private String mSearchText;
    private final int mSpanCount = 3;
    private final int mSpacing = 24;
    private int mCityCode;
    private List<CityDataInfo> mSearchResultList = new ArrayList<>();

    public OfflineSearchPoiList(@NonNull final Context context) {
        super(context);
    }

    public OfflineSearchPoiList(@NonNull final Context context, @Nullable final AttributeSet attrs) {
        super(context, attrs);

    }

    public OfflineSearchPoiList(@NonNull final Context context, @Nullable final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);

    }

    @Override
    protected OfflineSearchResultViewBinding createViewBinding(final LayoutInflater inflater, final ViewGroup viewGroup) {
        return OfflineSearchResultViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected OfflineSearchPoiListImpl initSceneImpl() {
        return new OfflineSearchPoiListImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setISceneSugSearchPoiList(mScreenViewModel);
    }

    @Override
    protected void initObserver() {
        setupRecyclerView();
        setupSearchActions();
        requestFocusAndShowKeyboard();
    }
    public void setSearchText(final String searchText) {
        this.mSearchText = searchText;
    }
    /**
     * 初始化 RecyclerView
     */
    private void setupRecyclerView() {
        final LinearLayoutManager layoutManager = new LinearLayoutManager(getContext());
        layoutManager.setOrientation(LinearLayoutManager.VERTICAL);
        mViewBinding.offlineRecyclerSearchResult.setLayoutManager(layoutManager);

        mAdapter = new OfflineProvincesAdapter(getContext(), new ArrayList<>());
        mViewBinding.offlineRecyclerSearchResult.setAdapter(mAdapter);
        mAdapter.setData(MapDataPackage.getInstance().getAllDownLoadedList());
        if (ConvertUtils.isEmpty(MapDataPackage.getInstance().getAllDownLoadedList())) {
            mViewBinding.searchNoCityHint.setVisibility(View.VISIBLE);
        } else {
            mViewBinding.searchNoCityHint.setVisibility(View.GONE);
        }
        mAdapter.setListener(new OfflineCitiesAdapter.ItemClickListener() {
            @Override
            public void onClick(final int cityCode) {
                mCityCode = cityCode;
                mScreenViewModel.keywordSearch(1, mSearchText, cityCode, true);
            }
        });

        final GridLayoutManager searchLinearLayoutManager = new GridLayoutManager(getContext(), mSpanCount);
        mSearchAdapter = new OfflineSearchResultAdapter(getContext(), new ArrayList<>());
        mViewBinding.offlineRecyclerSearchKeywordResult.setLayoutManager(searchLinearLayoutManager);
        mViewBinding.offlineRecyclerSearchKeywordResult.addItemDecoration(new GridSpacingItemDecoration(
                getContext(), mSpanCount, mSpacing, mSpacing, false));

        mViewBinding.offlineRecyclerSearchKeywordResult.setAdapter(mSearchAdapter);
        mSearchAdapter.setListener(new OfflineSearchResultAdapter.ItemClickListener() {
            @Override
            public void onClick(final int cityCode) {
                mCityCode = cityCode;
                mScreenViewModel.keywordSearch(1, mSearchText, cityCode, true);
            }
        });
        mViewBinding.offlineRecyclerSearchKeywordResult.setVisibility(View.GONE);
    }

    /**
     * 搜索相关事件
     */
    private void setupSearchActions() {
        mViewBinding.offlineSearchTopView.ivClose.setOnClickListener(v -> mScreenViewModel.closeSearch());
        mViewBinding.offlineSearchTopView.searchBarEditView.setHint(ResourceUtils.Companion.getInstance().getString(R.string.offline_search_hint));
        mViewBinding.offlineSearchTopView.searchBarEditView.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(final CharSequence s, final int start, final int count, final int after) {

            }

            @Override
            public void onTextChanged(final CharSequence s, final int start, final int before, final int count) {

            }

            @Override
            public void afterTextChanged(final Editable editable) {
                if (!editable.toString().trim().isEmpty()) {
                    mViewBinding.offlineSearchTopView.ivEditClear.setVisibility(VISIBLE);
                    mSearchResultList = convertSearchResultList(MapDataPackage.getInstance().searchDownLoaded(
                            editable.toString().trim()), editable.toString().trim());
                    mSearchAdapter.setData(mSearchResultList);
                    mSearchAdapter.notifyDataSetChanged();
                    if (ConvertUtils.isEmpty(mSearchResultList)) {
                        mViewBinding.searchNoResultHint.setText(getContext().getString(
                                R.string.search_offline_no_result_hint, editable.toString().trim()));
                        mViewBinding.searchNoCityHint.setVisibility(View.GONE);
                    } else {
                        mViewBinding.searchNoResultHint.setText("");
                    }
                    mViewBinding.offlineRecyclerSearchKeywordResult.setVisibility(View.VISIBLE);
                    mViewBinding.offlineRecyclerSearchResult.setVisibility(GONE);
                } else {
                    mViewBinding.offlineSearchTopView.ivEditClear.setVisibility(View.GONE);
                    mViewBinding.offlineRecyclerSearchKeywordResult.setVisibility(View.GONE);
                    mViewBinding.offlineRecyclerSearchResult.setVisibility(View.VISIBLE);
                    mViewBinding.searchNoResultHint.setText("");
                }
            }
        });

        mViewBinding.offlineSearchTopView.searchBarEditView.setOnEditorActionListener((v, actionId, event) -> {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onEditorActionListener actionId: " + actionId);
            mSearchResultList = convertSearchResultList(MapDataPackage.getInstance().searchDownLoaded(getEditText()), getEditText());
            mSearchAdapter.setData(mSearchResultList);
            mSearchAdapter.notifyDataSetChanged();
            if (ConvertUtils.isEmpty(mSearchResultList)) {
                mViewBinding.searchNoResultHint.setText(getContext().getString(R.string.search_offline_no_result_hint, getEditText()));
                mViewBinding.searchNoCityHint.setVisibility(View.GONE);
            } else {
                mViewBinding.searchNoResultHint.setText("");
            }
            mViewBinding.offlineRecyclerSearchKeywordResult.setVisibility(View.VISIBLE);
            mViewBinding.offlineRecyclerSearchResult.setVisibility(GONE);
            hideInput();
            return true;
        });

        mViewBinding.offlineSearchTopView.ivEditClear.setOnClickListener(view -> clearEditText());
    }

    /**
     * 把ProvDataInfo List转换成CityDataInfo List
     * @param originList 原始数据列表
     * @param keyword 目标搜索关键字
     * @return 转换后的结果列表
     */
    private List<CityDataInfo> convertSearchResultList(final List<ProvDataInfo> originList, final String keyword) {
        final List<CityDataInfo> searchResultList = new ArrayList<>();
        for (ProvDataInfo provDataInfo : originList) {
            if (provDataInfo.getName().contains(keyword)) {
                final CityDataInfo cityDataInfo = new CityDataInfo();
                cityDataInfo.setName(provDataInfo.getName());
                cityDataInfo.setAdcode(provDataInfo.getAdcode());
                searchResultList.add(cityDataInfo);
            }
            if (!ConvertUtils.isEmpty(provDataInfo.getCityInfoList())) {
                for (CityDataInfo cityDataInfo : provDataInfo.getCityInfoList()) {
                    if (cityDataInfo.getName().contains(keyword)) {
                        searchResultList.add(cityDataInfo);
                    }
                }
            }
        }
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "convertSearchResultList keyword: " + keyword
                + ", searchResultList size: " + searchResultList.size());
        return searchResultList;
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
     * @param searchResultEntity 搜索结果实体
     */
    public void notifySearchResult(final int taskId,final SearchResultEntity searchResultEntity) {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "taskId: " + taskId
                + " currentId: " + mScreenViewModel.getMTaskId());
        if (!ConvertUtils.equals(taskId, mScreenViewModel.getMTaskId()) && mScreenViewModel.getMTaskId() != 0) {
            return;
        }

        if (searchResultEntity == null || searchResultEntity.getPoiList().isEmpty()) {
            //若未搜索到数据，弹出提示
            ToastUtils.Companion.getInstance().showCustomToastView("抱歉，未找到结果");
            return;
        }
        //否则跳转搜索结果页面展示数据
        final Fragment fragment = (Fragment) ARouter.getInstance()
                .build(RoutePath.Search.SEARCH_RESULT_FRAGMENT)
                .navigation();
        final String sourceFragment = AutoMapConstant.SourceFragment.FRAGMENT_OFFLINE;
        final Bundle bundle = SearchFragmentFactory.createKeywordFragment(
                sourceFragment, AutoMapConstant.SearchType.SEARCH_KEYWORD, searchResultEntity.getKeyword(), null);
        bundle.putInt("cityCode", mCityCode);
        addFragment((BaseFragment) fragment, bundle);

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
        return mViewBinding.offlineSearchTopView.searchBarEditView.getText() != null ?
                mViewBinding.offlineSearchTopView.searchBarEditView.getText().toString().trim() : "";
    }

    /**
     * 清空输入框内容
     */
    public void clearEditText() {
        mViewBinding.offlineSearchTopView.searchBarEditView.setText("");
        if (null != mAdapter) {
            mAdapter.setData(MapDataPackage.getInstance().getAllDownLoadedList());
            if (ConvertUtils.isEmpty(MapDataPackage.getInstance().getAllDownLoadedList())) {
                mViewBinding.searchNoCityHint.setVisibility(View.VISIBLE);
            } else {
                mViewBinding.searchNoCityHint.setVisibility(View.GONE);
            }
            mViewBinding.offlineRecyclerSearchKeywordResult.setVisibility(View.GONE);
            mViewBinding.offlineRecyclerSearchResult.setVisibility(View.VISIBLE);
            mViewBinding.searchNoResultHint.setText("");
        }
    }

    /**
     * 请求焦点并显示软键盘
     */
    public void requestFocusAndShowKeyboard() {
        // 确保视图已经附加到窗口
        mViewBinding.offlineSearchTopView.searchBarEditView.post(() -> {
            mViewBinding.offlineSearchTopView.searchBarEditView.requestFocus();
            showKeyboard();
        });
    }

    /**
     * 显示软键盘
     */
    private void showKeyboard() {
        final InputMethodManager imm = (InputMethodManager) getContext().getSystemService(Context.INPUT_METHOD_SERVICE);
        if (imm != null) {
            imm.showSoftInput(mViewBinding.offlineSearchTopView.searchBarEditView, InputMethodManager.SHOW_IMPLICIT);
        }
    }
}