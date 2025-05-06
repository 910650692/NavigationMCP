package com.fy.navi.scene.ui.adapter;


import android.annotation.SuppressLint;
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
import com.fy.navi.scene.api.search.IOnFilterChildItemClickListener;
import com.fy.navi.scene.databinding.FilterChildItemBinding;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.search.SearchChildCategoryLocalInfo;

import java.util.ArrayList;
import java.util.List;

public class FilterChildListAdapter extends RecyclerView.Adapter<FilterChildListAdapter.Holder> {
    private final List<SearchChildCategoryLocalInfo> mSearchCategoryLocalInfos;
    private IOnFilterChildItemClickListener mFilterItemClickListener;
    private boolean mIsCollapse = true;

    public boolean isCollapse() {
        return mIsCollapse;
    }

    public void setCollapse(final boolean collapse) {
        mIsCollapse = collapse;
    }
    public FilterChildListAdapter() {
        mSearchCategoryLocalInfos = new ArrayList<>();
    }

    public void setFilterItemClickListener(final IOnFilterChildItemClickListener filterItemClickListener) {
        this.mFilterItemClickListener = filterItemClickListener;
    }

    /**
     * 设置分类列表
     * @param infos 分类列表
     */
    public void setCategoryList(final List<SearchChildCategoryLocalInfo> infos) {
        if (ConvertUtils.isEmpty(infos)) {
            mSearchCategoryLocalInfos.clear();
            notifyDataSetChanged();
            return;
        }
        mSearchCategoryLocalInfos.clear();
        mSearchCategoryLocalInfos.addAll(infos);
        notifyDataSetChanged();
    }

    @Override
    public Holder onCreateViewHolder(@NonNull final ViewGroup parent, final int viewType) {
        final FilterChildItemBinding filterItemBinding =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()),
                        R.layout.filter_child_item, parent, false);
        return new Holder(filterItemBinding);
    }

    @Override
    public int getItemCount() {
        if (mSearchCategoryLocalInfos == null) {
            return 0;
        }
        if(mSearchCategoryLocalInfos.isEmpty()){
            return 0;
        }
        if (mIsCollapse) {
            return Math.min(mSearchCategoryLocalInfos.size(), 6);
        }
        return mSearchCategoryLocalInfos.size() + 1;
    }

    @Override
    public void onBindViewHolder(@NonNull final Holder holder, @SuppressLint("RecyclerView")final int position) {
        if (position == mSearchCategoryLocalInfos.size()) {
            holder.mfilterItemBinding.filterChildImg.setVisibility(View.VISIBLE);
            holder.mfilterItemBinding.filterChildImg.setImageResource(R.drawable.img_up_48);
            holder.mfilterItemBinding.filterChildText.setVisibility(View.GONE);
            holder.mfilterItemBinding.getRoot().setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(final View view) {
                    mIsCollapse = true;
                    notifyDataSetChanged();
                }
            });
            return;
        }
        final SearchChildCategoryLocalInfo localInfo = mSearchCategoryLocalInfos.get(position);
        if (localInfo == null) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onBindViewHolder localInfo is null");
            return;
        }
        if (mIsCollapse && position == 5 && mSearchCategoryLocalInfos.size() > 6) {
            holder.mfilterItemBinding.filterChildImg.setVisibility(View.VISIBLE);
            holder.mfilterItemBinding.filterChildText.setVisibility(View.GONE);
            holder.mfilterItemBinding.getRoot().setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(final View view) {
                    mIsCollapse = false;
                    notifyDataSetChanged();
                }
            });
        } else {
            holder.mfilterItemBinding.filterChildImg.setVisibility(View.GONE);
            holder.mfilterItemBinding.filterChildText.setVisibility(View.VISIBLE);
            holder.mfilterItemBinding.filterChildText.setText(localInfo.getName());
            holder.mfilterItemBinding.filterChildText.setSelected(localInfo.getChecked() == 1);
            final ColorStateList colorStateList = ContextCompat.getColorStateList(holder.mfilterItemBinding.filterChildText.getContext(),
                    R.color.custom_filter_item_text_bg_selector_day);
            holder.mfilterItemBinding.filterChildText.setTextColor(colorStateList);
            holder.mfilterItemBinding.filterChildRoot.setSelected(localInfo.getChecked() == 1);
            holder.mfilterItemBinding.getRoot().setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(final View view) {
                    if (mFilterItemClickListener != null) {
                        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onClick childItem position: " + position
                                + " ,name: " + localInfo.getName());
                        refreshItemCheckedState(position);
                        mFilterItemClickListener.onItemClick(position);
                    }
                    notifyDataSetChanged();
                }
            });
        }
    }

    /**
     * 刷新item的选中状态
     * @param position 点击的位置
     */
    private void refreshItemCheckedState(final int position) {
        //如果点击位置和选中的位置相同，无操作
        if (mSearchCategoryLocalInfos.get(position).getChecked() == 1) {
            return;
        }
        //如果点击位置和选中的位置不同，更新选中状态
        for (int i = 0; i < mSearchCategoryLocalInfos.size(); i++) {
            if (i == position) {
                mSearchCategoryLocalInfos.get(i).setChecked(1);
            } else {
                mSearchCategoryLocalInfos.get(i).setChecked(-1);
            }
        }
    }

    public static class Holder extends RecyclerView.ViewHolder {
        private final FilterChildItemBinding mfilterItemBinding;

        public Holder(final FilterChildItemBinding filterItemBinding) {
            super(filterItemBinding.getRoot());
            this.mfilterItemBinding = filterItemBinding;
            filterItemBinding.setHolder(this);
        }
    }
}