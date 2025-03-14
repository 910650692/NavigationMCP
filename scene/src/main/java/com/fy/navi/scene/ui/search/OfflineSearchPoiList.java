package com.fy.navi.scene.ui.search;

import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_HMI_TAG;

import android.content.Context;
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

import com.android.utils.ConvertUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.R;
import com.fy.navi.scene.adapter.GridSpacingItemDecoration;
import com.fy.navi.scene.databinding.OfflineSearchResultViewBinding;
import com.fy.navi.scene.impl.search.OfflineSearchPoiListImpl;
import com.fy.navi.scene.ui.adapter.OfflineCitiesAdapter;
import com.fy.navi.scene.ui.adapter.OfflineProvincesAdapter;
import com.fy.navi.scene.ui.adapter.OfflineSearchResultAdapter;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.mapdata.ProvDataInfo;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.logicpaket.mapdata.MapDataPackage;

import java.util.ArrayList;
import java.util.List;

/**
 * @Author: baipeng0904
 * @Description: 联想搜索结果列表 scene
 * @Date: 2020/4/16 11:05 AM
 * @CreateDate: $ $
 */
public class OfflineSearchPoiList extends BaseSceneView<OfflineSearchResultViewBinding, OfflineSearchPoiListImpl> {
    private OfflineProvincesAdapter mAdapter;
    private OfflineSearchResultAdapter mSearchAdapter;
    private String searchText;
    private int spanCount = 3;
    private int spacing = 24;
    private List<CityDataInfo> searchResultList = new ArrayList<>();

    public OfflineSearchPoiList(@NonNull Context context) {
        super(context);
    }

    public OfflineSearchPoiList(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);

    }

    public OfflineSearchPoiList(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);

    }

    @Override
    protected OfflineSearchResultViewBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
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
    public void setSearchText(String searchText) {
        this.searchText = searchText;
    }
    /**
     * 初始化 RecyclerView
     */
    private void setupRecyclerView() {
        LinearLayoutManager layoutManager = new LinearLayoutManager(getContext());
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
            public void onClick(int cityCode) {
                mScreenViewModel.keywordSearch(1, searchText, cityCode);
            }
        });

        GridLayoutManager searchLinearLayoutManager = new GridLayoutManager(getContext(), spanCount);
        mSearchAdapter = new OfflineSearchResultAdapter(getContext(), new ArrayList<>());
        mViewBinding.offlineRecyclerSearchKeywordResult.setLayoutManager(searchLinearLayoutManager);
        mViewBinding.offlineRecyclerSearchKeywordResult.addItemDecoration(new GridSpacingItemDecoration(getContext(), spanCount, spacing, spacing, false));

        mViewBinding.offlineRecyclerSearchKeywordResult.setAdapter(mSearchAdapter);
        mSearchAdapter.setListener(new OfflineSearchResultAdapter.ItemClickListener() {
            @Override
            public void onClick(int cityCode) {
                mScreenViewModel.keywordSearch(1, searchText, cityCode);
            }
        });
        mViewBinding.offlineRecyclerSearchKeywordResult.setVisibility(View.GONE);
    }

    /**
     * 搜索相关事件
     */
    private void setupSearchActions() {
        mViewBinding.offlineSearchTopView.ivClose.setOnClickListener(v -> mScreenViewModel.closeSearch());

        mViewBinding.offlineSearchTopView.searchBarEditView.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {

            }

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {

            }

            @Override
            public void afterTextChanged(Editable editable) {
                if (!editable.toString().trim().isEmpty()) {
                    mViewBinding.offlineSearchTopView.ivEditClear.setVisibility(VISIBLE);
                    searchResultList = convertSearchResultList(MapDataPackage.getInstance().searchDownLoaded(editable.toString().trim()), editable.toString().trim());
                    mSearchAdapter.setData(searchResultList);
                    mSearchAdapter.notifyDataSetChanged();
                    if (ConvertUtils.isEmpty(searchResultList)) {
                        mViewBinding.searchNoResultHint.setText(getContext().getString(R.string.search_offline_no_result_hint, editable.toString().trim()));
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
            Logger.d(SEARCH_HMI_TAG, "onEditorActionListener actionId: " + actionId);
            searchResultList = convertSearchResultList(MapDataPackage.getInstance().searchDownLoaded(getEditText()), getEditText());
            mSearchAdapter.setData(searchResultList);
            mSearchAdapter.notifyDataSetChanged();
            if (ConvertUtils.isEmpty(searchResultList)) {
                mViewBinding.searchNoResultHint.setText(getContext().getString(R.string.search_offline_no_result_hint, getEditText()));
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

    private List<CityDataInfo> convertSearchResultList(List<ProvDataInfo> originList, String keyword) {
        List<CityDataInfo> searchResultList = new ArrayList<>();
        for (ProvDataInfo provDataInfo : originList) {
            if (provDataInfo.name.contains(keyword)) {
                CityDataInfo cityDataInfo = new CityDataInfo();
                cityDataInfo.name = provDataInfo.name;
                cityDataInfo.adcode = provDataInfo.adcode;
                searchResultList.add(cityDataInfo);
            }
            if (!ConvertUtils.isEmpty(provDataInfo.cityInfoList)) {
                for (CityDataInfo cityDataInfo : provDataInfo.cityInfoList) {
                    if (cityDataInfo.name.contains(keyword)) {
                        searchResultList.add(cityDataInfo);
                    }
                }
            }
        }
        Logger.d(SEARCH_HMI_TAG, "convertSearchResultList keyword: " + keyword
                + ", searchResultList size: " + searchResultList.size());
        return searchResultList;
    }

    /**
     * 执行预搜索
     */
    public void suggestionSearch(String keyword) {
        Logger.d(SEARCH_HMI_TAG, "sugSearch search: " + ", Keyword: " + keyword);
        mScreenViewModel.suggestionSearch(keyword);
    }

    /**
     * 更新搜索结果
     */
    public void notifySearchResult(SearchResultEntity searchResultEntity) {
        if (searchResultEntity == null || searchResultEntity.getPoiList().isEmpty()) {
            //若未搜索到数据，弹出提示
            ToastUtils.Companion.getInstance().showCustomToastView("抱歉，未找到结果");
            return;
        }
        //否则回到搜索页面展示搜索结果
        closeAllFragmentsUntilTargetFragment("OfflineSearchFragment");
    }

    /**
     * 隐藏软键盘
     */
    public void hideInput() {
        InputMethodManager imm = (InputMethodManager) getContext().getSystemService(Context.INPUT_METHOD_SERVICE);
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
        return mViewBinding.offlineSearchTopView.searchBarEditView.getText() != null ? mViewBinding.offlineSearchTopView.searchBarEditView.getText().toString().trim() : "";
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
        InputMethodManager imm = (InputMethodManager) getContext().getSystemService(Context.INPUT_METHOD_SERVICE);
        if (imm != null) {
            imm.showSoftInput(mViewBinding.offlineSearchTopView.searchBarEditView, InputMethodManager.SHOW_IMPLICIT);
        }
    }
}