package com.fy.navi.scene.ui.adapter;

import android.content.res.TypedArray;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.fy.navi.scene.R;
import com.fy.navi.scene.api.search.IQuickSearchItemClickListener;
import com.fy.navi.scene.databinding.QuickSearchItemBinding;

public class QuickSearchListAdapter extends RecyclerView.Adapter<QuickSearchListAdapter.ResultHolder> {
    private String[] categories;
    private TypedArray icons;
    private IQuickSearchItemClickListener onItemClickListener;

    public void setOnItemClickListener(IQuickSearchItemClickListener listener) {
        onItemClickListener = listener;
    }

    public void setCategories(TypedArray icons, String[] categories) {
        if (this.icons != null) {
            this.icons.recycle();
        }
        this.icons = icons;
        this.categories = categories;
        notifyItemRangeChanged(0, getItemCount());
    }

    @NonNull
    @Override
    public ResultHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        QuickSearchItemBinding quickSearchItemBinding = DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()), R.layout.quick_search_item, parent, false);
        return new ResultHolder(quickSearchItemBinding);
    }

    @Override
    public void onBindViewHolder(@NonNull ResultHolder holder, int position) {
        if (categories != null && categories.length > position) {
            holder.resultItemBinding.quickName.setText(categories[position]);
        }
        if (icons != null && icons.length() > position) {
            holder.resultItemBinding.skIvIcon.setImageResource(icons.getResourceId(position, 0));
        }

        holder.resultItemBinding.getRoot().setOnClickListener(v -> {
            if (onItemClickListener != null) {
                onItemClickListener.onItemClick(position, categories[position]);
            }
        });
    }

    @Override
    public int getItemCount() {
        return categories != null ? categories.length : 0;
    }

    @Override
    public void onDetachedFromRecyclerView(@NonNull RecyclerView recyclerView) {
        super.onDetachedFromRecyclerView(recyclerView);
        if (icons != null) {
            icons.recycle();
            icons = null;
        }
    }

    public static class ResultHolder extends RecyclerView.ViewHolder {
        public QuickSearchItemBinding resultItemBinding;

        public ResultHolder(QuickSearchItemBinding resultItemBinding) {
            super(resultItemBinding.getRoot());
            this.resultItemBinding = resultItemBinding;
            this.resultItemBinding.setHolder(this);
        }
    }
}
