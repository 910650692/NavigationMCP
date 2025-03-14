package com.fy.navi.scene.ui.search;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.android.utils.ToastUtils;
import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.databinding.TerminalParkingResultViewBinding;
import com.fy.navi.scene.impl.search.SceneTerminalViewImpl;
import com.fy.navi.scene.ui.adapter.TerminalParkingResultAdapter;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;

/**
 * @Author: baipeng0904
 * @Description: SceneTerminalParkingListView
 * @CreateDate: 2025/3/5 16:32
 */
public class SceneTerminalParkingListView extends BaseSceneView<TerminalParkingResultViewBinding, SceneTerminalViewImpl> {
    private TerminalParkingResultAdapter mAdapter;

    public SceneTerminalParkingListView(@NonNull Context context) {
        super(context);
    }

    public SceneTerminalParkingListView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneTerminalParkingListView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected TerminalParkingResultViewBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
        return TerminalParkingResultViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneTerminalViewImpl initSceneImpl() {
        return new SceneTerminalViewImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setSceneTerminalViewImpl(mScreenViewModel);
    }

    @Override
    protected void initObserver() {
        setupRecyclerView();
        setupSearchActions();

    }

    public void aroundSearch(GeoPoint geoPoint) {
        mScreenViewModel.aroundSearch("停车场", geoPoint);
    }

    /**
     * 初始化 RecyclerView
     */
    private void setupRecyclerView() {
        LinearLayoutManager layoutManager = new LinearLayoutManager(getContext());
        layoutManager.setOrientation(LinearLayoutManager.VERTICAL);
        mViewBinding.recyclerSearchResult.setLayoutManager(layoutManager);

        mAdapter = new TerminalParkingResultAdapter();
        mViewBinding.recyclerSearchResult.setAdapter(mAdapter);
        mAdapter.setOnItemClickListener(new TerminalParkingResultAdapter.OnItemClickListener() {
            @Override
            public void onItemClick(int position, PoiInfoEntity poiInfoEntity) {
                // TODO 终点停车场事件
                mScreenViewModel.startRoute(poiInfoEntity);
            }

            @Override
            public void onNaviClick(int position, PoiInfoEntity poiInfoEntity) {
                // TODO 终点停车场事件
                mScreenViewModel.startRoute(poiInfoEntity);
            }
        });
    }

    /**
     * 搜索相关事件
     */
    private void setupSearchActions() {
        mViewBinding.skIvTerminalClose.setOnClickListener(v -> mScreenViewModel.closeSearch());
    }

    /**
     * 更新搜索结果
     */
    public void notifySearchResult(SearchResultEntity searchResultEntity) {
        if (searchResultEntity == null || searchResultEntity.getPoiList().isEmpty()) {
            ToastUtils.Companion.getInstance().showCustomToastView("暂无数据");
            return;
        }
        if (mAdapter != null) {
            mAdapter.notifyList(searchResultEntity);
        }
    }
}
