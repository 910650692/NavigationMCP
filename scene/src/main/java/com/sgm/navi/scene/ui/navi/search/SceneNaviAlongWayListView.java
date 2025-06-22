package com.sgm.navi.scene.ui.navi.search;

import android.content.Context;
import android.content.res.TypedArray;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.recyclerview.widget.GridLayoutManager;

import com.android.utils.log.Logger;
import com.sgm.navi.burypoint.anno.HookMethod;
import com.sgm.navi.burypoint.bean.BuryProperty;
import com.sgm.navi.burypoint.constant.BuryConstant;
import com.sgm.navi.burypoint.controller.BuryPointController;
import com.sgm.navi.scene.BaseSceneView;
import com.sgm.navi.scene.api.navi.ISceneNaviSearchView;
import com.sgm.navi.scene.databinding.SceneNaviAlongWayListViewBinding;
import com.sgm.navi.scene.impl.navi.search.SceneNaviAlongWayListViewImpl;
import com.sgm.navi.scene.ui.adapter.QuickSearchListAdapter;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.logicpaket.navi.OpenApiHelper;

public class SceneNaviAlongWayListView extends BaseSceneView<SceneNaviAlongWayListViewBinding,
        SceneNaviAlongWayListViewImpl> {

    public static final String TAG = "SceneNaviAlongWayListView";

    private QuickSearchListAdapter mQuickSearchListAdapter;

    private ISceneNaviSearchView mISceneNaviSearchView;

    private PoiInfoEntity mPoiInfoEntity;
    private int mSearchType;

    public SceneNaviAlongWayListView(@NonNull Context context) {
        super(context);
    }

    public SceneNaviAlongWayListView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviAlongWayListView(@NonNull Context context, @Nullable AttributeSet attrs,
                                     int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected SceneNaviAlongWayListViewBinding createViewBinding(LayoutInflater inflater,
                                                                 ViewGroup viewGroup) {
        return SceneNaviAlongWayListViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneNaviAlongWayListViewImpl initSceneImpl() {
        return new SceneNaviAlongWayListViewImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setNaviSearch(mScreenViewModel);
    }

    @Override
    protected void initObserver() {
        setupRecyclerView();
        setupAdapterListener();
    }

    /**
     * 初始化recyclerView
     */
    private void setupRecyclerView() {
        final GridLayoutManager layoutManager = new GridLayoutManager(getContext(), 4);
        layoutManager.setOrientation(GridLayoutManager.VERTICAL);
        mViewBinding.skRcvQuick.setLayoutManager(layoutManager);
        if (null == mQuickSearchListAdapter) {
            mQuickSearchListAdapter = new QuickSearchListAdapter();
        }
        mViewBinding.skRcvQuick.setAdapter(mQuickSearchListAdapter);
    }

    /**
     * 设置adapter的点击事件
     */
    private void setupAdapterListener() {
        if (mQuickSearchListAdapter != null) {
            mQuickSearchListAdapter.setOnItemClickListener((position, name) -> {
                // 只有沿途搜
                Logger.i(TAG, "Along way search");
                mISceneNaviSearchView.goSearchResultView(name, OpenApiHelper.ALONG_WAY);
                sendBuryPointEvent(name);
            });
        } else {
            Logger.i(TAG, "quickSearchListAdapter is null");
        }
    }

    @HookMethod(eventName = BuryConstant.EventName.AMAP_NAVI_POI_ROUTE_SELECT)
    private void sendBuryPointEvent(String name) {
        BuryProperty buryProperty = new BuryProperty.Builder()
                .setParams(BuryConstant.ProperType.BURY_KEY_SEARCH_CONTENTS, name)
                .build();
        BuryPointController.getInstance().setBuryProps(buryProperty);
    }

    /**
     * 设置快捷搜索列表的数据
     * @param iconArray 图标数组
     * @param categories 类别数组
     */
    public void setQuickSearchListAdapterData(final TypedArray iconArray, final String[] categories) {
        if (null == mQuickSearchListAdapter) {
            mQuickSearchListAdapter = new QuickSearchListAdapter();
        }
        mQuickSearchListAdapter.setCategories(iconArray, categories);
    }

    public void setSceneNaviSearchView(ISceneNaviSearchView sceneNaviSearchView) {
        mISceneNaviSearchView = sceneNaviSearchView;
    }

    public void closeSearchView() {
        mISceneNaviSearchView.closeSearchView();
    }
}
