package com.fy.navi.scene.ui.adapter;

import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_HMI_TAG;

import android.annotation.SuppressLint;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.R;
import com.fy.navi.scene.api.search.IOnFilterItemClickListener;
import com.fy.navi.scene.databinding.FilterItemBinding;
import com.fy.navi.scene.databinding.GasStationItemBinding;
import com.fy.navi.service.define.search.GasStationInfo;
import com.fy.navi.service.define.search.SearchCategoryLocalInfo;
import com.fy.navi.service.define.search.SearchChildCategoryLocalInfo;
import com.fy.navi.service.define.utils.NumberUtils;

import java.util.ArrayList;
import java.util.List;

public class FilterListAdapter extends RecyclerView.Adapter<FilterListAdapter.Holder> {
    private List<SearchChildCategoryLocalInfo> mSearchCategoryLocalInfos;

    private IOnFilterItemClickListener filterItemClickListener;
    private boolean isExpand = false;
    private String currentExpandName = "";

    public void setFilterItemClickListener(IOnFilterItemClickListener filterItemClickListener) {
        this.filterItemClickListener = filterItemClickListener;
    }
    public FilterListAdapter() {
        mSearchCategoryLocalInfos = new ArrayList<>();
    }

    public void setCategoryList(List<SearchChildCategoryLocalInfo> infos) {
        if (ConvertUtils.isEmpty(infos)) {
            return;
        }
        mSearchCategoryLocalInfos.clear();
        mSearchCategoryLocalInfos.addAll(infos);
        notifyDataSetChanged();
    }

    @Override
    public Holder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        FilterItemBinding filterItemBinding =
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
    public void onBindViewHolder(@NonNull Holder holder, @SuppressLint("RecyclerView") int position) {
        SearchChildCategoryLocalInfo localInfo = mSearchCategoryLocalInfos.get(position);
        if (localInfo == null) {
            Logger.d(SEARCH_HMI_TAG, "onBindViewHolder localInfo is null");
            return;
        }
        holder.filterItemBinding.filterText.setText(localInfo.getName());
        holder.filterItemBinding.filterText.setSelected(localInfo.getChecked() == 1);
        holder.filterItemBinding.filterRoot.setSelected(localInfo.getChecked() == 1);
        holder.filterItemBinding.filterImg.setImageResource(ConvertUtils.equals(currentExpandName,
                localInfo.getName()) ? R.drawable.img_up_48 : R.drawable.img_under_the_48);
        if (localInfo.getCategoryLocalInfos() != null && localInfo.getCategoryLocalInfos().size() > 0) {
            holder.filterItemBinding.filterImg.setVisibility(View.VISIBLE);
        } else {
            holder.filterItemBinding.filterImg.setVisibility(View.GONE);
        }

        holder.filterItemBinding.getRoot().setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                if (filterItemClickListener != null) {
                    Logger.d(SEARCH_HMI_TAG, "onClick item position: " + position
                            + " ,name: " + localInfo.getName());
                    refreshItemCheckedState(position);
                    if (localInfo.getCategoryLocalInfos() != null && localInfo.getCategoryLocalInfos().size() > 0) {
                        //有三级菜单，点击展开或收起
                        if (!isExpand) {
                            //折叠状态，点击有子列表的item项，直接展开
                            isExpand = true;
                            currentExpandName = localInfo.getName();
                            filterItemClickListener.onChildListExpandCollapse(localInfo.getCategoryLocalInfos(), position);

                        } else {
                            //展开状态，如果点击的是当前展开的item项，收起子列表
                            if (!ConvertUtils.isEmpty(currentExpandName)
                                    && ConvertUtils.equals(currentExpandName, localInfo.getName())) {
                                isExpand = false;
                                currentExpandName = "";
                                filterItemClickListener.onChildListExpandCollapse(null, position);
                            } else {
                                //展开状态，如果点击的不是当前展开的item项，展开新的子列表，收起旧的子列表
                                isExpand = true;
                                currentExpandName = localInfo.getName();
                                filterItemClickListener.onChildListExpandCollapse(localInfo.getCategoryLocalInfos(), position);
                            }
                        }
                    } else {
                        //没有三级菜单，点击选中并触发搜索逻辑
                        currentExpandName = "";
                        filterItemClickListener.onItemClick(position);
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
    private void refreshItemCheckedState(int position) {
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
//            Logger.d(SEARCH_HMI_TAG, "mSearchCategoryLocalInfos000 : " + mSearchCategoryLocalInfos.get(i));
        }
    }

    public class Holder extends RecyclerView.ViewHolder {
        public FilterItemBinding filterItemBinding;

        public Holder(FilterItemBinding filterItemBinding) {
            super(filterItemBinding.getRoot());
            this.filterItemBinding = filterItemBinding;
            filterItemBinding.setHolder(this);
        }
    }
}