#[derive(Copy, Clone, Debug)]
pub enum Shape {
    None,
    Box,
    Polygon,
    Ellipse,
    Oval,
    Circle,
    DoubleCircle,
    Point,
    Egg,
    Triangle,
    PlainText,
    Plain,
    Diamond,
    Trapezium,
}

impl std::fmt::Display for Shape {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::None => "none",
                Self::Box => "box",
                Self::Polygon => "polygon",
                Self::Ellipse => "ellipse",
                Self::Oval => "oval",
                Self::Circle => "circle",
                Self::DoubleCircle => "doublecircle",
                Self::Point => "point",
                Self::Egg => "egg",
                Self::Triangle => "triangle",
                Self::PlainText => "plaintext",
                Self::Plain => "plain",
                Self::Diamond => "diamond",
                Self::Trapezium => "trapezium",
            }
        )
    }
}

#[derive(Clone, Default, Debug)]
pub struct Style {
    fontname: Option<String>,
    label: Option<String>,
    shape: Option<Shape>,
    height: Option<f32>,
    width: Option<f32>,
}

impl std::fmt::Display for Style {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let label = self.label.clone().map(|s| format!("label=\"{s}\""));
        let fontname = self.fontname.clone().map(|s| format!("fontname={s}"));
        let shape = self.shape.map(|s| format!("shape={s}"));
        let height = self.height.map(|s| format!("height={s}"));
        let width = self.width.map(|s| format!("width={s}"));

        let s = [label, fontname, shape, height, width]
            .into_iter()
            .flatten()
            .collect::<Vec<_>>()
            .join(", ");

        write!(f, "{s}")
    }
}

impl Style {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn fontname(mut self, fontname: impl ToString) -> Self {
        self.fontname = Some(fontname.to_string());
        self
    }

    pub fn label(mut self, label: impl ToString) -> Self {
        self.label = Some(label.to_string());
        self
    }

    pub fn shape(mut self, shape: Shape) -> Self {
        self.shape = Some(shape);
        self
    }

    pub fn height(mut self, height: f32) -> Self {
        self.height = Some(height);
        self
    }

    pub fn width(mut self, width: f32) -> Self {
        self.width = Some(width);
        self
    }

    pub fn is_styled(&self) -> bool {
        self.label.is_some()
            || self.shape.is_some()
            || self.height.is_some()
            || self.width.is_some()
    }
}

#[derive(Clone, Debug)]
pub struct Vertex {
    id: String,
    style: Style,
}

impl std::fmt::Display for Vertex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.style.is_styled() {
            write!(f, "{} [{}]", self.id, self.style)
        } else {
            write!(f, "{}", self.id)
        }
    }
}

#[derive(Clone, Debug)]
pub struct Edge {
    from: String,
    to: String,
    style: Style,
}

impl std::fmt::Display for Edge {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.style.is_styled() {
            write!(f, "{} -> {} [{}]", self.from, self.to, self.style)
        } else {
            write!(f, "{} -> {}", self.from, self.to)
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum RankDir {
    TopBottom,
    BottomTop,
    LeftRight,
    RightLeft,
}

impl std::fmt::Display for RankDir {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::TopBottom => "TB",
                Self::BottomTop => "BT",
                Self::LeftRight => "LR",
                Self::RightLeft => "RL",
            }
        )
    }
}

#[derive(Debug)]
pub struct DiGraph {
    name: String,
    rankdir: Option<RankDir>,
    vertices: Vec<Vertex>,
    edges: Vec<Edge>,
}

impl DiGraph {
    pub fn new(name: impl ToString) -> Self {
        Self {
            name: name.to_string(),
            vertices: Vec::default(),
            rankdir: None,
            edges: Vec::default(),
        }
    }

    pub fn rankdir(&mut self, dir: RankDir) -> &mut Self {
        self.rankdir = Some(dir);
        self
    }

    pub fn vertex(&mut self, id: impl ToString, style: impl Into<Option<Style>>) -> &mut Self {
        self.vertices.push(Vertex {
            id: id.to_string(),
            style: style.into().unwrap_or_default(),
        });

        self
    }

    pub fn edge(
        &mut self,
        from: impl ToString,
        to: impl ToString,
        style: impl Into<Option<Style>>,
    ) -> &mut Self {
        self.edges.push(Edge {
            from: from.to_string(),
            to: to.to_string(),
            style: style.into().unwrap_or_default(),
        });

        self
    }
}

impl std::fmt::Display for DiGraph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let vertices = self
            .vertices
            .iter()
            .map(|v| v.to_string())
            .collect::<Vec<_>>()
            .join("\n");

        let edges = self
            .edges
            .iter()
            .map(|v| v.to_string())
            .collect::<Vec<_>>()
            .join("\n");

        write!(
            f,
            "digraph {} {{ 
        {}
        {vertices}\n
        {edges}\
        }}",
            self.name,
            match self.rankdir {
                Some(rd) => format!("rankdir={rd}"),
                None => "".to_string(),
            }
        )
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn builder_compiles() {
        let _ = DiGraph::new("test graph")
            .vertex("a", None)
            .vertex("b", Style::new().label("Boo!").shape(Shape::Diamond))
            .edge("a", "b", None);
    }
}
