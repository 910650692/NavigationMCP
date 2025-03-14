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
import com.fy.navi.scene.api.search.IOnFilterChildItemClickListener;
import com.fy.navi.scene.api.search.IOnFilterItemClickListener;
import com.fy.navi.scene.databinding.FilterChildItemBinding;
import com.fy.navi.scene.databinding.FilterItemBinding;
import com.fy.navi.service.define.search.SearchCategoryLocalInfo;
import com.fy.navi.service.define.search.SearchChildCategoryLocalInfo;
import com.fy.navi.service.define.utils.NumberUtils;

import java.util.ArrayList;
import java.util.List;

public class FilterChildListAdapter extends RecyclerView.Adapter<FilterChildListAdapter.Holder> {
    private List<SearchChildCategoryLocalInfo> mSearchCategoryLocalInfos;
    private IOnFilterChildItemClickListener filterItemClickListener;
    private boolean isCollapse = true;

    public boolean isCollapse() {
        return isCollapse;
    }

    public void setCollapse(boolean collapse) {
        isCollapse = collapse;
    }
    public FilterChildListAdapter() {
        mSearchCategoryLocalInfos = new ArrayList<>();
    }

    public void setFilterItemClickListener(IOnFilterChildItemClickListener filterItemClickListener) {
        this.filterItemClickListener = filterItemClickListener;
    }

    public void setCategoryList(List<SearchChildCategoryLocalInfo> infos) {
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
    public Holder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        FilterChildItemBinding filterItemBinding =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()),
                        R.layout.filter_child_item, parent, false);
        return new Holder(filterItemBinding);
    }

    @Override
    public int getItemCount() {
        if (mSearchCategoryLocalInfos == null) {
            return 0;
        }
        if (isCollapse) {
            return Math.min(mSearchCategoryLocalInfos.size(), 6);
        }
        return mSearchCategoryLocalInfos.size() + 1;
    }

    @Override
    public void onBindViewHolder(@NonNull Holder holder, @SuppressLint("RecyclerView") int position) {
        if (position == mSearchCategoryLocalInfos.size()) {
            holder.filterItemBinding.filterChildImg.setVisibility(View.VISIBLE);
            holder.filterItemBinding.filterChildImg.setImageResource(R.drawable.img_up_48);
            holder.filterItemBinding.filterChildText.setVisibility(View.GONE);
            holder.filterItemBinding.getRoot().setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View view) {
                    isCollapse = true;
                    notifyDataSetChanged();
                }
            });
            return;
        }
        SearchChildCategoryLocalInfo localInfo = mSearchCategoryLocalInfos.get(position);
        if (localInfo == null) {
            Logger.d(SEARCH_HMI_TAG, "onBindViewHolder localInfo is null");
            return;
        }
        if (isCollapse && position == 5 && mSearchCategoryLocalInfos.size() > 6) {
            holder.filterItemBinding.filterChildImg.setVisibility(View.VISIBLE);
            holder.filterItemBinding.filterChildText.setVisibility(View.GONE);
            holder.filterItemBinding.getRoot().setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View view) {
                    isCollapse = false;
                    notifyDataSetChanged();
                }
            });
        } else {
            holder.filterItemBinding.filterChildImg.setVisibility(View.GONE);
            holder.filterItemBinding.filterChildText.setVisibility(View.VISIBLE);
            holder.filterItemBinding.filterChildText.setText(localInfo.getName());
            holder.filterItemBinding.filterChildText.setSelected(localInfo.getChecked() == 1);
            holder.filterItemBinding.filterChildRoot.setSelected(localInfo.getChecked() == 1);
            holder.filterItemBinding.getRoot().setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View view) {
                    if (filterItemClickListener != null) {
                        Logger.d(SEARCH_HMI_TAG, "onClick childItem position: " + position
                                + " ,name: " + localInfo.getName());
                        refreshItemCheckedState(position);
                        filterItemClickListener.onItemClick(position);
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
        }
    }

    public class Holder extends RecyclerView.ViewHolder {
        public FilterChildItemBinding filterItemBinding;

        public Holder(FilterChildItemBinding filterItemBinding) {
            super(filterItemBinding.getRoot());
            this.filterItemBinding = filterItemBinding;
            filterItemBinding.setHolder(this);
        }
    }
}