package com.fy.navi.scene.ui.adapter;


import android.content.res.ColorStateList;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.core.content.ContextCompat;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.ListScenePoiDetailsScenicChildSpotBinding;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.search.ChildInfo;

import java.util.ArrayList;
import java.util.List;

public class PoiListDetailsScenicChildAdapter extends RecyclerView.Adapter<PoiListDetailsScenicChildAdapter.Holder> {
    private final List<ChildInfo> mChildList;
    private OnItemClickListener mItemClickListener;
    private boolean mIsCollapse = true;

    public boolean isCollapse() {
        return mIsCollapse;
    }

    public void setCollapse(final boolean collapse) {
        mIsCollapse = collapse;
    }

    public PoiListDetailsScenicChildAdapter() {
        mChildList = new ArrayList<>();
    }


    /**
     * 设置子poi点数据
     * @param childInfoList poi点数据
     */
    public void setChildInfoList(final List<ChildInfo> childInfoList) {
        if (ConvertUtils.isEmpty(childInfoList)) {
            return;
        }

        mChildList.clear();
        mChildList.addAll(childInfoList);
        notifyDataSetChanged();
    }

    public void setItemClickListener(final OnItemClickListener itemClickListener) {
        this.mItemClickListener = itemClickListener;
    }

    @Override
    public Holder onCreateViewHolder(@NonNull final ViewGroup parent, final int viewType) {
        final ListScenePoiDetailsScenicChildSpotBinding scenePoiDetailsScenicChildSpotBinding =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()),
                        R.layout.list_scene_poi_details_scenic_child_spot, parent, false);
        return new Holder(scenePoiDetailsScenicChildSpotBinding);
    }

    @Override
    public int getItemCount() {
        if (ConvertUtils.isEmpty(mChildList)) {
            return 0;
        }
        if (mIsCollapse) {
            return Math.min(mChildList.size(), 2);
        }
        return mChildList.size();
    }

    @Override
    public void onBindViewHolder(@NonNull final Holder holder, final int position) {
        final ChildInfo childInfo = mChildList.get(position);
        final int ratio = (int) Math.round(childInfo.getRatio());
        final String subTitle = holder.mScenePoiDetailsScenicChildSpotBinding.childTitle.getContext().
                getString(R.string.list_scenic_ratio, ratio);
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "check = " + childInfo.getChecked() + " name: " + childInfo.getShortName());
        holder.mScenePoiDetailsScenicChildSpotBinding.childTitle.setText(childInfo.getShortName());
        final ColorStateList titleList = ContextCompat.getColorStateList(holder.mScenePoiDetailsScenicChildSpotBinding.childTitle.getContext(),
                R.color.custom_filter_item_text_bg_selector_day);
        holder.mScenePoiDetailsScenicChildSpotBinding.childTitle.setTextColor(titleList);

        holder.mScenePoiDetailsScenicChildSpotBinding.childSubTitle.setText(subTitle);
        final ColorStateList subTitleList = ContextCompat.getColorStateList(holder.mScenePoiDetailsScenicChildSpotBinding.childSubTitle.getContext(),
                R.color.custom_filter_sub_item_text_bg_selector_day);
        holder.mScenePoiDetailsScenicChildSpotBinding.childSubTitle.setTextColor(subTitleList);

        holder.mScenePoiDetailsScenicChildSpotBinding.childTitle.setSelected(childInfo.getChecked() == 1);
        holder.mScenePoiDetailsScenicChildSpotBinding.childSubTitle.setSelected(childInfo.getChecked() == 1);
        holder.mScenePoiDetailsScenicChildSpotBinding.poiChildLayout.setSelected(childInfo.getChecked() == 1);
        holder.mScenePoiDetailsScenicChildSpotBinding.getRoot().setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(final View view) {
                if (mItemClickListener != null) {
                    boolean isSelect = false;
                    for (int i = 0; i < mChildList.size(); i++) {
                        if (i == position) {
                            final ChildInfo tempInfo = mChildList.get(i);
                            if (tempInfo.getChecked() == 1) {
                                mChildList.get(i).setChecked(-1);
                                isSelect = false;
                            } else {
                                mChildList.get(i).setChecked(1);
                                isSelect = true;
                            }
                        } else {
                            mChildList.get(i).setChecked(-1);
                        }
                    }
                    //isSelect判断是选中还是非选中，非选中则取消子点高亮
                    mItemClickListener.onItemClick(position, isSelect);
                }
                notifyDataSetChanged();
            }
        });
    }

    public static class Holder extends RecyclerView.ViewHolder {
        private final ListScenePoiDetailsScenicChildSpotBinding mScenePoiDetailsScenicChildSpotBinding;

        public Holder(final ListScenePoiDetailsScenicChildSpotBinding scenePoiDetailsScenicChildSpotBinding) {
            super(scenePoiDetailsScenicChildSpotBinding.getRoot());
            this.mScenePoiDetailsScenicChildSpotBinding = scenePoiDetailsScenicChildSpotBinding;
            scenePoiDetailsScenicChildSpotBinding.setHolder(this);
        }
    }

    public interface OnItemClickListener {
        /**
         * 点击事件
         * @param index 点击下标
         * @param isSelectIndex 是否被选中
         */
        void onItemClick(int index, boolean isSelectIndex);
    }
}