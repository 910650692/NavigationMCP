package com.sgm.navi.scene.ui.adapter;


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
import com.android.utils.ResourceUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.scene.BuildConfig;
import com.sgm.navi.scene.R;
import com.sgm.navi.scene.api.search.IOnFilterItemClickListener;
import com.sgm.navi.scene.databinding.FilterItemBinding;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.search.SearchChildCategoryLocalInfo;
import com.sgm.navi.service.define.utils.NumberUtils;

import java.util.ArrayList;
import java.util.List;

public class FilterListAdapter extends RecyclerView.Adapter<FilterListAdapter.Holder> {
    private final List<SearchChildCategoryLocalInfo> mSearchCategoryLocalInfos;

    private IOnFilterItemClickListener mFilterItemClickListener;
    private boolean mIsExpand = false;
    private String mCurrentExpandName = "";

    public void setMIsExpand(final boolean isExpand) {
        this.mIsExpand = isExpand;
    }
    public void setMCurrentExpandName(final String name) {
        this.mCurrentExpandName = name;
    }
    public void setFilterItemClickListener(final IOnFilterItemClickListener filterItemClickListener) {
        this.mFilterItemClickListener = filterItemClickListener;
    }
    public FilterListAdapter() {
        mSearchCategoryLocalInfos = new ArrayList<>();
    }

    /**
     * 设置分类列表
     * @param infos 分类列表
     */
    public void setCategoryList(final List<SearchChildCategoryLocalInfo> infos) {
        if (ConvertUtils.isEmpty(infos)) {
            return;
        }
        mSearchCategoryLocalInfos.clear();
        mSearchCategoryLocalInfos.addAll(infos);
        notifyDataSetChanged();
    }

    @Override
    public Holder onCreateViewHolder(@NonNull final ViewGroup parent, final int viewType) {
        final FilterItemBinding filterItemBinding =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()),
                        R.layout.filter_item, parent, false);
        return new Holder(filterItemBinding);
    }

    @Override
    public int getItemCount() {
        if (ConvertUtils.isEmpty(mSearchCategoryLocalInfos)) {
            return NumberUtils.NUM_0;
        }
        return mSearchCategoryLocalInfos.size();
    }

    @Override
    public void onBindViewHolder(@NonNull final Holder holder, @SuppressLint("RecyclerView") final int position) {
        final SearchChildCategoryLocalInfo localInfo = mSearchCategoryLocalInfos.get(position);
        if (localInfo == null) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onBindViewHolder localInfo is null");
            return;
        }
        holder.mFilterItemBinding.filterText.setSelected(false);
        holder.mFilterItemBinding.filterRoot.setSelected(false);
        String filterTitle = "";
        String filterTitleMore = ResourceUtils.Companion.getInstance().getString(R.string.filter_select_more);
        if(!ConvertUtils.isEmpty(localInfo.getName())){
            filterTitle = localInfo.getName().endsWith(filterTitleMore) ? localInfo.getName().replace(filterTitleMore,"") : localInfo.getName();
        }
        holder.mFilterItemBinding.filterText.setText(filterTitle);
        if (BuildConfig.FLAVOR.equals("clea_8255")
                || BuildConfig.FLAVOR.equals("clea_local_8155")
                || BuildConfig.FLAVOR.equals("clea_8775")) {
            holder.mFilterItemBinding.filterImg.setImageResource(ConvertUtils.equals(mCurrentExpandName,
                    localInfo.getName()) ? R.drawable.img_up_36 : R.drawable.img_under_the_36);
        }else{
            holder.mFilterItemBinding.filterImg.setImageResource(ConvertUtils.equals(mCurrentExpandName,
                    localInfo.getName()) ? R.drawable.img_up_48 : R.drawable.img_under_the_48);
        }
        if (localInfo.getCategoryLocalInfos() != null && !localInfo.getCategoryLocalInfos().isEmpty()) {
            holder.mFilterItemBinding.filterImg.setVisibility(View.VISIBLE);
        } else {
            holder.mFilterItemBinding.filterImg.setVisibility(View.GONE);
            holder.mFilterItemBinding.filterText.setSelected(localInfo.getChecked() == 1);
            final ColorStateList colorStateList = ContextCompat.getColorStateList(holder.mFilterItemBinding.filterText.getContext(),
                    R.color.custom_filter_item_text_bg_selector_day);
            holder.mFilterItemBinding.filterText.setTextColor(colorStateList);
            holder.mFilterItemBinding.filterRoot.setSelected(localInfo.getChecked() == 1);
        }

        holder.mFilterItemBinding.getRoot().setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(final View view) {
                if (mFilterItemClickListener != null) {
                    Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onClick item position: " + position
                            + " ,name: " + localInfo.getName());
                    refreshItemCheckedState(position);
                    if (localInfo.getCategoryLocalInfos() != null && !localInfo.getCategoryLocalInfos().isEmpty()) {
                        //有三级菜单，点击展开或收起
                        if (!mIsExpand) {
                            //折叠状态，点击有子列表的item项，直接展开
                            mIsExpand = true;
                            mCurrentExpandName = localInfo.getName();
                            mFilterItemClickListener.onChildListExpandCollapse(localInfo.getCategoryLocalInfos(), position);

                        } else {
                            //展开状态，如果点击的是当前展开的item项，收起子列表
                            if (!ConvertUtils.isEmpty(mCurrentExpandName)
                                    && ConvertUtils.equals(mCurrentExpandName, localInfo.getName())) {
                                mIsExpand = false;
                                mCurrentExpandName = "";
                                mFilterItemClickListener.onChildListExpandCollapse(null, position);
                            } else {
                                //展开状态，如果点击的不是当前展开的item项，展开新的子列表，收起旧的子列表
                                mIsExpand = true;
                                mCurrentExpandName = localInfo.getName();
                                mFilterItemClickListener.onChildListExpandCollapse(localInfo.getCategoryLocalInfos(), position);
                            }
                        }
                    } else {
                        //没有三级菜单，点击选中并触发搜索逻辑
                        mCurrentExpandName = "";
                        mFilterItemClickListener.onItemClick(position);
                    }
                }
                notifyDataSetChanged();
            }
        });
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
        private final FilterItemBinding mFilterItemBinding;

        public Holder(final FilterItemBinding filterItemBinding) {
            super(filterItemBinding.getRoot());
            this.mFilterItemBinding = filterItemBinding;
            filterItemBinding.setHolder(this);
        }
    }
}