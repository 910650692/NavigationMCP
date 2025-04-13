package com.fy.navi.scene.ui.poi;

import android.content.Context;
import android.graphics.Rect;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.recyclerview.widget.DividerItemDecoration;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.adapter.HorizontalSpaceItemDecoration;
import com.fy.navi.scene.databinding.ChargePriceListViewBinding;
import com.fy.navi.scene.impl.poi.ScenePoiChargePriceListViewImpl;
import com.fy.navi.scene.ui.adapter.ChargePriceListAdapter;
import com.fy.navi.service.define.search.ChargePriceInfo;

import java.util.List;

public class ScenePoiChargePriceListView extends BaseSceneView<ChargePriceListViewBinding, ScenePoiChargePriceListViewImpl> {
    private ChargePriceListAdapter mAdapter;

    public ScenePoiChargePriceListView(@NonNull Context context) {
        super(context);
    }

    public ScenePoiChargePriceListView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public ScenePoiChargePriceListView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected ChargePriceListViewBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
        return ChargePriceListViewBinding.inflate(inflater,viewGroup,true);
    }

    @Override
    protected ScenePoiChargePriceListViewImpl initSceneImpl() {
        return new ScenePoiChargePriceListViewImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setScenePoiChargePriceListViewImpl(mScreenViewModel);
    }

    @Override
    protected void initObserver() {
        initAdapter();
    }

    private void initAdapter() {
        final int spacingInPixelsTop = getResources().getDimensionPixelSize(com.fy.navi.ui.R.dimen.dp_16);
        final int spacingInPixelsLeft = getResources().getDimensionPixelSize(com.fy.navi.ui.R.dimen.dp_40);
        final LinearLayoutManager layoutManager = new LinearLayoutManager(getContext());
        layoutManager.setOrientation(LinearLayoutManager.VERTICAL);
        mViewBinding.recyclerChargePrice.setLayoutManager(layoutManager);
        mAdapter = new ChargePriceListAdapter();
        mViewBinding.recyclerChargePrice.setAdapter(mAdapter);
        mViewBinding.recyclerChargePrice.addItemDecoration(new RecyclerView.ItemDecoration() {
            @Override
            public void getItemOffsets(@NonNull Rect outRect, @NonNull View view, @NonNull RecyclerView parent, @NonNull RecyclerView.State state) {
                outRect.top = spacingInPixelsTop;
                outRect.left = spacingInPixelsLeft;
            }
        });
    }

    // 通知全价格列表
    public void notifyChargePriceList(List<ChargePriceInfo> list){
        mAdapter.notifyList(list);
    }

    // 关闭当前页面
    public void closeFragment(){
        closeCurrentFragment();
    }
}
